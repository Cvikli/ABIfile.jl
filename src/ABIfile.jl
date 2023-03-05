module ABIfile

using Revise
using JSON
using HTTP


include("keccak.jl")




global verbose = false

####################
# ABI
####################

# COMMON functions
getABI(contract_address::String, etherscan_api_key::String) = HTTP.get("https://api.etherscan.io/api?module=contract&action=getabi&address=$contract_address&apikey=$etherscan_api_key")




# TYPES

include("Types.jl")

struct Decl{ENCMODE, X, Y, Z}
	name::String
	typename::String
	components::Array
	indexed::Bool
end

struct ABIFunction
	hash # first 4 bytes of keccak hash
	name
	inputs::Array{Decl}
	outputs::Array{Decl}
	statemutability
end
struct ABIEvent
	hash
	name
	inputs::Array{Decl}
	anonymous
end
struct ABIError
	hash
	name
	inputs::Array{Decl}
end

struct Contract{ADDRESS}
	id::String
	functions #::Array{ABIFunction}
	events    #::Array{ABIEvent}
	function Contract(id::String)
			id = cleanaddress(id)
			new{Symbol(id)}(id, Dict{Union{String, Vector{UInt8}}, ABIFunction}(), Dict())
	end
end


function cleanaddress(str::String)
	if match(r"^0[xX]", str) != nothing
			str = str[3:end]
	end
	@assert match(r"[0-9a-fA-F]{20}", str) != nothing
	str
end








parseABI(json) = begin
	
end

# Given an event name and series of event parameters, we split them into two sub-series: those which are indexed and those which are not. Those which are indexed, which may number up to 3 (for non-anonymous events) or 4 (for anonymous ones), are used alongside the Keccak hash of the event signature to form the topics of the log entry. 
args_signature(json) = join([inp["type"] for inp in json["inputs"] if get(inp, "indexed", false)==true][1:min(end, get(json, "anonymous", false) ? 3 : 4)], ",") 

parseABI(json) = (println(get(json, "type", "function")),parseABI(Val(Symbol(get(json, "type", "function"))), json))
parseABI(::FunctionABI, func) = begin
	type = get(json, "type", "function")
	if type == "function"
		func = json
		name = func["name"]
		inputs  = !haskey(func, "inputs")  || length(func["inputs"])  === 0 ? () : parseargs(func["inputs"])
		outputs = !haskey(func, "outputs") || length(func["outputs"]) === 0 ? () : parseargs(func["inputs"])

		sig = "$name($(args_signature(func)))"

		return ABIFunction(
				hash(sig)[1:4],
				name,
				inputs,
				outputs,
				func["stateMutability"],
		)
	elseif type == "event"
		evt = json
		sig = "$name($(args_signature(func)))"

		return ABIEvent(
			hash(sig)[1:4], 
			evt["name"], 
			parseargs(evt["inputs"]),
			get(evt, "anonymous", false), 
		)
	elseif type == "error"
		error = json
		sig = "$name($(args_signature(error)))"

		return ABIError(
			hash(sig)[1:4], 
			error["name"], 
			parseargs(evt["inputs"]),
		)
	else 
		@assert false "unknown type" 
	end
end

function parseABI(::Val{:event}, evt)
end

computetype(decl::Decl{T, :int,  SIZE}) where {T, SIZE} = soliditytojulia["int$SIZE"]
computetype(decl::Decl{T, :uint, SIZE}) where {T, SIZE} = soliditytojulia["uint$SIZE"]
computetype(decl::Decl{:tuple, SIZE})   where {T, SIZE} = "tuple[$SIZE]"
computetype(decl::Decl{:array, BASE, BITS, LENGTH}) where {BASE, BITS, LENGTH} = "array[LENGTH] of $BASE"
computetype(decl::Decl{T, :bool} where {T}) = Bool
computetype(decl::Decl{:string}) = String
computetype(decl::Decl{:bytes})  = Vector{UInt8}
computetype(decl::Decl{:dynamic, TYPE}) where TYPE = "array of $TYPE"

computetypes(func::ABIFunction) = computetypes(func.name, func.inputs)
function computetypes(name, decls::Array{T} where T <: Decl)
	computetype.(decls)
end

"""
	readABI(contractname::String, stream::IO)

Read an ABI file for a contract
"""
function readABI(contractname::String, stream::IO)
	contract = Contract(contractname)
	d = JSON.parse(stream)
	close(stream)
	d = ((isa(d, Dict) && haskey(d, "abi")) ? d["abi"] : d)
	for json_abi in d
		@show json_abi
		obj = parseABI(json_abi)
		@show obj
		if isa(obj, ABIFunction)
				contract.functions[obj.name] = contract.functions[obj.hash] = obj
				if verbose
						println("$(bytes2hex(obj.hash)) $(obj.signature) $(repr(obj)) $(bytes2hex(hash(obj.signature)))")
				end
		elseif isa(obj, ABIEvent)
				contract.events[obj.name] = contract.events[obj.hash] = obj
				if verbose
						println("$(bytes2hex(obj.hash)) $(obj.signature) $(repr(obj)) $(bytes2hex(hash(obj.signature)))")
				end
		elseif verbose
				println(repr(obj))
		end
	end
	contracts[contractname] = contract
end

function basetypefor(typename)
	m = match(bitspattern, typename)
	(Symbol(m[1]), m[2] == "" ? 256 : parse(Int, m[2]))
end

function typefor(typename, arg)
	atype = arg["type"]
	if endswith(atype, "[]")
			(:dynamic, basetypefor(typename)..., :none)
	elseif endswith(atype, "]")
			m = match(fixedarraypattern, typename)
			(:array, basetypefor(typename)..., parse(Int, m[1]))
	elseif atype == "tuple"
			(:tuple, length(arg["components"]), :none, :none)
	elseif atype in ["string", "bytes"]
			(Symbol(atype), :none, :none, :none)
	else
			(:scalar, basetypefor(typename)..., :none)
	end
end

function parsearg(arg)
	typename = arg["type"]
	(enctype, atype, bits, len) = typefor(typename, arg)
	Decl{enctype, atype, bits, len}(
			arg["name"],
			typename,
			haskey(arg, "components") ? parseargs(arg["components"]) : [],
			get(arg, "indexed", false)
	)
end

parseargs(args) = parsearg.(args)















const NumDecl = Union{Decl{T, :int} where T, Decl{T, :uint} where T}
const FunctionABI = Union{Val{:function}, Val{:constructor}, Val{:fallback}};

"A dictionary of contract-address => Contract structures"
const contracts = Dict()

const fixedarraypattern = r".*\[([^\]]+)\]"
const bitspattern = r"^([^[0-9]+)([0-9]*)"

##############
# ENCODING
##############

"""
	encodefunctioncall(f::ABIFunction, inputs::Array) -> data
	encodefunctioncall(io::IO, f::ABIFunction, inputs::Array)
	encodefunctioncall(io::IOBuffer, f::ABIFunction, inputs::Array) -> data

Encode a call to a function
"""
encodefunctioncall(f::ABIFunction, inputs::Array) = encodefunctioncall(IOBuffer(), f, inputs)
encodefunctioncall(io::IO, f::ABIFunction, inputs::Array) = basicencodefunctioncall(io, f, inputs)
function encodefunctioncall(io::IOBuffer, f::ABIFunction, inputs::Array)
	basicencodefunctioncall(io, f, inputs)
	take!(io)
end

function basicencodefunctioncall(io::IO, f::ABIFunction, inputs::Array)
	if length(f.inputs) != length(inputs)
			throw("Wrong number of inputs to $(f.name), expecting $(length(f.inputs)) but got $(length(inputs))")
	end
	write(io, f.hash)
	encode(io, f.inputs, inputs)
end

"""
	encodefunctionresult(io::IO, f::ABIFunction, outputs::Array)

Encode the results of a function
"""
function encodefunctionresult(io::IO, f::ABIFunction, outputs::Array)
	basicencodefunctionresult(io, f, outputs)
end

function encodefunctionresult(io::IOBuffer, f::ABIFunction, outputs::Array)
	basicencodefunctionresult(io, f, outputs)
	io.data
end

function basicencodefunctionresult(io::IO, f::ABIFunction, outputs::Array)
	write(io, f.hash)
	encode(io, f.outputs, outputs)
end

"""
	encodeevent(io::IO, e::ABIEvent, inputs::Array)

Encode an event
"""
function encodeevent(io::IO, e::ABIEvent, inputs::Array)
	basicencodeevent(io, e, inputs)
end

function encodeevent(io::IOBuffer, e::ABIEvent, inputs::Array)
	basicencodeevent(io, e, inputs)
	io.data
end

function basicencodeevent(io::IO, e::ABIEvent, inputs::Array)
	write(io, e.hash)
	encode(io, e.inputs, inputs)
end

function encode(io::IO, decl::Union{Decl, Array}, value)
	encodehead(io, decl, value, length(value) * 32)
	encodetail(io, decl, value)
end

encint(io, i::Signed) = encints(Int128(i < 0 ? -1 : 0), Int128(i))
encint(io, i::Unsigned) = encints(io, UInt128(0), UInt128(i))
encint(io, i::Union{Int256, UInt256}) = encints(i.big, i.little)
encints(io, ints...) = vcat(hton.(ints)...)

# utilities
writeint(io, i::Signed) = writeints(io, Int128(i < 0 ? -1 : 0), Int128(i))
writeint(io, i::Unsigned) = writeints(io, UInt128(0), UInt128(i))
writeint(io, i::Union{Int256, UInt256}) = writeints(io, i.big, i.little)
writeints(io, ints...) = write(io, hton.(ints)...)
encodescalar(io::IO, decl::Decl{:scalar, :bool}, value) = writeint(io, value ? 1 : 0)
encodescalar(io::IO, decl::NumDecl, value) = writeint(io, value)

# scalar types
function encodehead(io::IO, decl::Decl{:scalar}, v, offset::Int)
	encodescalar(io, decl, v)
	offset
end
encodetail(io, ::Decl{:scalar}, v) = nothing

# dynamic types
function encodehead(io::IO, ::Union{Decl{:string}, Decl{:bytes}}, v, offset::Int)::Int
	writeint(io, offset)
	offset + ceil(length(IOBuffer(v).data) / 32) * 32 + 32
end
function encodetail(io, ::Union{Decl{:string}, Decl{:bytes}}, v)
	buf = IOBuffer(v)
	len = bytesavailable(buf)
	writeint(io, len)
	write(io, v)
	pad = 32 - len % 32
	if pad != 32
			for i = 1 : pad
					write(io, Int8(0))
			end
	end
end
function encodehead(io::IO, ::Decl{:dynamic}, v, offset::Int)
	writeint(io, offset)
	offset + length(v) * 64
end
function encodetail(io::IO, decl::Decl{:dynamic}, values)
	for v in values
			encodescalar(decl, v)
	end
end

# fixed-size array
function encodehead(io::IO, decl::Decl{:array, BASE, BITS, LENGTH}, values, offset::Int) where {BASE, BITS, LENGTH}
	writeint(io, offset)
	offset + LENGTH * 64
end
function encodetail(io::IO, decl::Decl{:array, BASE, BITS, LENGTH}, values) where {BASE, BITS, LENGTH}
	t = arraycomptype(decl)
	writeint(length)
	offset = 0
	for i in 1:LENGTH
			offset = encodehead(io, t, values[i], offset)
	end
end
function arraycomptype(decl::Decl{:array, BASE, BITS, LENGTH}) where {BASE, BITS, LENGTH}
	if decl.components != nothing
			Decl{:tuple, length(decl.components), :n, :n}(decl.name, "tuple", decl.components, false)
	else
			Decl{:scalar, BASE, BITS, :none}(decl.name, decl.typename, :nothing, false)
	end
end

# tuple
encodehead(io::IO, decl::Decl{:tuple}, values, offset) = encodehead(io, decl.components, values, offset)
encodetail(io::IO, decl::Decl{:tuple}, values) = encodetail(io, decl.components, values)
function encodehead(io::IO, decls::Array{Decl}, values, offset::Int)
	for i in 1:length(decls)
			offset = encodehead(io, decls[i], values[i], offset)
	end
	offset
end
function encodetail(io::IO, decls::Array{Decl}, values)
	for i in 1:length(decls)
			encodetail(io, decls[i], values[i])
	end
end

############
# DECODING
############

const signedTypes   = Dict([sizeof(t)*8 => t for t in (Int8, Int16, Int32, Int64, Int128)])
const unsignedTypes = Dict([sizeof(t)*8 => t for t in (UInt8, UInt16, UInt32, UInt64, UInt128)])

function readint(io::IO)
	big = read(io, Int128)
	Int256(ntoh(big), ntoh(read(io, UInt128)))
end

function readuint(io::IO)
	big = read(io, UInt128)
	Int256(ntoh(big), ntoh(read(io, UInt128)))
end

readlength(io::IO) = (read(io, UInt128);ntoh(read(io, UInt128)))

"""
	decodefunctioncall(io::IO, con::Contract)

Decode a function call
"""
function decodefunctioncall(io::IO, con::Contract)
	decl = con.functions[read(io, 4)]
	FunctionCall(decl, decode(io, decl.inputs))
end

"""
	decodefunctionresult(io::IO, con::Contract)

Decode a function call result
"""
function decodefunctionresult(io::IO, con::Contract)
	hash = read(io, 4)
	f = con.functions[hash]
	FunctionResult(f, decode(io, f.outputs))
end

"""
	decodeevent(io::IO, con::Contract)

Decode an event in a transaction log
"""
function decodeevent(io::IO, con::Contract)
	decl = con.events[read(io, 4)]
	Event(decl, decode(io, decl.inputs))
end

# general
decode(io::IO, decl) = decodetail(io, decl, decodehead(io, decl))

# scalar types
decodehead(io::IO, decl::Decl{:scalar}) = decodescalar(io, decl)
decodetail(io::IO, ::Decl{:scalar}, head) = head
function decodescalar(io::IO, ::Decl{T, :int, SIZE}) where {T, SIZE}
	big = ntoh(read(io, Int128))
	little = ntoh(read(io, UInt128))
	SIZE <= 128 ? smallint(SIZE, big < 0 ? -little : little) : Int256(big, little)
end
function decodescalar(io::IO, ::Decl{T, :uint, SIZE}) where {T, SIZE}
	big = ntoh(read(io, UInt128))
	little = ntoh(read(io, UInt128))
	SIZE <= 128 ? smalluint(SIZE, little) : UInt256(big, little)
end

smallint(size, value) = signedTypes[Int(2^ceil(log2(floor((33 + 7) / 8))))](value)
smalulint(size, value) = unsignedTypes[Int(2^ceil(log2(floor((33 + 7) / 8))))](value)

function decode(io::IO, ::Decl{T, :bool}) where T
	big = read(io, UInt128)
	little = read(io, UInt128)
	litle == 0 ? false : true
end

# dynamic types
decodehead(io::IO, decl::Decl{:dynamic, :bytes}) = readlength(io)
decodetail(io::IO, decl::Decl{:dynamic, :bytes}, head) = readbytes(io, head)
readbytes(io::IO, len) = read(io, len)

function decode(io::IO, decl::Decl{:string})
	if verbose println("Reading string at $(position(io))") end
	offset = readlength(io)
	pos = position(io)
	if verbose println("Offset: $(offset)") end
	seek(io, offset)
	count = readlength(io)
	if verbose println("Length: $(count)") end
	result = String(read(io, count))
	seek(io, pos)
	result
end

# array types
function decode(io::IO, decl::Decl{:array, BASE, BITS, LENGTH}) where {BASE, BITS, LENGTH}
	[decodescalar(io, decl) for i in 1:LENGTH]
end
function decode(io::IO, ::Decl{:array, :bytes, BITS, LENGTH}) where {BITS, LENGTH}
	lens = [readlength(io) for i in 1:LENGTH]
	[readbytes(io, len) for len in lens]
end
function decode(io::IO, ::Decl{:array, :string, BITS, LENGTH}) where {BITS, LENGTH}
	lens = [readlength(io) for i in 1:LENGTH]
	[String(readbytes(io, len)) for len in lens]
end
function decode(io::IO, decl::Decl{:array, :tuple, BITS, LENGTH}) where {BITS, LENGTH}
	decodetail(io::IO, decl.components, decodehead(io::IO, decl.components))
end

# tuple
decode(io::IO, decl::Decl{:tuple}) = decodetail(io, decl.components)
#decode(io::IO, decls::Array) = decodetail(io, decls, decodehead(io, decls))
#decodehead(io::IO, decls::Array) = [decodehead(io, head) for head in decls]
#decodetail(io::IO, decls::Array, heads) = [decodetail(io, decls[i], heads[i]) for i in 1:length(decls)]
function decode(io::IO, decls::Array)
	result = []
	for decl in decls
			push!(result, decode(io, decl))
	end
	result
end

####################
# DECL PARSING
####################

rows(array) = [array[row, :] for row in 1:size(a)[1]]

hash(str::String) = keccak256(collect(UInt8, str))

####################
# UTILS
####################


# connection(con::ContractContext) = getfield(con, :connection)

# contract(con::ContractContext) = getfield(con, :contract)

# functions(con::ContractContext) = contract(con).functions


function gen(contract, con)
	funcs = collect(filter(p-> isa(p[1], String), contract.functions))
	funcdict = Dict(map(funcs) do ((name, func)) name => func end)
	methods = map(funcs) do ((name, func))
			argnames = map(a-> Symbol("a$a"), 1:length(func.argtypes))
			args = map(((name, type)::Tuple)-> :($name::$type), zip(argnames, func.argtypes))
			method = :($(Symbol(name)) = (
					send = ($(args...),; options...)-> send(context, $name, [$(argnames...)]; options...),
					call = ($(args...),; options...)-> call(context, $name, [$(argnames...)]; options...),
					estimategas = ($(args...),; options...)-> (),
					encodeabi = ($(args...),; options...)-> begin
							buf = IOBuffer()
							encodefunctioncall(buf, contract(context).functions[$(String(name))], [$(argnames...)])
							take!(buf)
					end
			))
			if verbose println("\n$name = ", method, "\n") end
			method
	end
	type = Contract{Symbol(contract.id)}
	eval(:(Base.getproperty(context::$type, prop::Symbol) = ($(methods...),)[prop]))
end


# function ContractContext(contractid::String, jsonabifile::String)
# 	ContractContext(contractid, jsonabifile)
# end
# ContractContext(contractid::String, jsonabifile::String) = ContractContext(contractid, open(jsonabifile))

# function ContractContext(contractid::String, file::IO)
# 	contract = readABI(contractid, file)
# 	gen(contract, con)
# 	ContractContext{Symbol(contract.id)}(con, contract)
# end

function setverbose(v)
	global verbose
	verbose = v
end


end # module ABIfile
#%%
