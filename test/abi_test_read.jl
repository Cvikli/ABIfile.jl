
using Revise
using ABIfile: getABI
using RelevanceStacktrace

abi = getABI("0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2", "K4W4UAYAFWATWIA9QE8N6YG5YCWKCQE2N7")

abi
#%%
String(abi)
#%%
#%%
abi = """[{"constant":false,"inputs":[{"name":"i","type":"int32"}],"name":"add","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"}]"""
contract = "0x694b0b0853c64aa51ebce186dd27bd676486cb43"

using ABIfile: readABI

readABI(contract, IOBuffer(String(abi)))


#%%
add
#%%

add(i::Int32) = (fn=:add, params=[i], output=Nothing)

#%%
"""[{"name": "f","type":"function",
"inputs":[
	{"name":"s","type":"tuple","components":[{"name":"a","type":"uint256" },{"name": "b","type": "uint256[]"},{"name":"c","type":"tuple[]","components":[{"name": "x","type": "uint256"},{"name":"y","type":"uint256"}]}]},
	{"name":"t","type":"tuple","components":[{"name":"x","type":"uint256"},{"name":"y","type":"uint256"}]},
	{"name":"a","type":"uint256"}],"outputs":[]}
]"""
f(s::Tuple{UInt256, Vector{UInt256}, Vector{Tuple{UInt256, UInt256}}}, t::Tuple{UInt256, UInt256}, a::UInt256) = begin
	return (fn=:f,
	params=[s, t, a],
	output=Nothing,
	payable=false,
	)
end
#%%
"""{"status":"1","message":"OK",
"result":"[{
	"constant":true,
	"inputs":[],
	"name":"name",
	"outputs":[{"name":"","type":"string"}],
	"payable":false,
	"stateMutability":"view",
	"type":"function"}"""
name() = (fn=:name,params=[],output=String,payable=false,stateMutability=:view,constant=true)
#%%
sii=:view
"$sii"
#%%
"""{
	"constant":false,
	"inputs":[{"name":"guy","type":"address"},{"name":"wad","type":"uint256"}],
	"name":"approve",
	"outputs":[{"name":"","type":"bool"}],
	"payable":false,
	"stateMutability":"nonpayable",
	"type":"function"}"""
approve(guy::Address) = (fn=:name,params=[],output=String,payable=false,stateMutability=:view,constant=true)
#%%

ssignedTypes   = ([sizeof(t)*8 => t for t in (UInt8, UInt16, Int32, Int64, Int128)])

#%%
a = NTuple{20, UInt8}(reinterpret.(UInt8, (0x1, 0x2, 0x3, 0x4, 0x5,0x1, 0x2, 0x3, 0x4, 0x5,0x1, 0x2, 0x3, 0x4, 0x5,0x1, 0x2, 0x3, 0x4, 0x5)))

sizeof(a)

#%%
a
#%%

sizeof(Int24)
sizeof(UInt24)
v = @edit Int32(1)

