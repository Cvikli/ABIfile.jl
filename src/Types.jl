

using BitIntegers

BitIntegers.@define_integers  24
BitIntegers.@define_integers  40
BitIntegers.@define_integers  48
BitIntegers.@define_integers  56
BitIntegers.@define_integers  72
BitIntegers.@define_integers  80
BitIntegers.@define_integers  88
BitIntegers.@define_integers  96
BitIntegers.@define_integers 104
BitIntegers.@define_integers 112
BitIntegers.@define_integers 120
BitIntegers.@define_integers 136
BitIntegers.@define_integers 144
BitIntegers.@define_integers 152
BitIntegers.@define_integers 160
BitIntegers.@define_integers 168
BitIntegers.@define_integers 176
BitIntegers.@define_integers 184
BitIntegers.@define_integers 192
BitIntegers.@define_integers 200
BitIntegers.@define_integers 208
BitIntegers.@define_integers 216
BitIntegers.@define_integers 224
BitIntegers.@define_integers 232
BitIntegers.@define_integers 240
BitIntegers.@define_integers 248


# Make conversion mapping given specs:
struct Bytes{SIZE} end
Bytes{SIZE}() where SIZE = NTuple{SIZE, UInt8} # VERY Ugly hack... But didn't know how to do it better.


const soliditytojulia = Dict(
	"int8"   => Int8, # most common
	"int16"  => Int16, # most common
	"int24"  => Int24,
	"int32"  => Int32, # most common
	"int40"  => Int40,
	"int48"  => Int48,
	"int56"  => Int56,
	"int64"  => Int64,
	"int72"  => Int72,
	"int80"  => Int80,
	"int88"  => Int88,
	"int96"  => Int96,
	"int104" => Int104,
	"int112" => Int112,
	"int120" => Int120,
	"int128" => Int128, # most common
	"int136" => Int136,
	"int144" => Int144,
	"int152" => Int152,
	"int160" => Int160, # most common
	"int168" => Int168,
	"int176" => Int176,
	"int184" => Int184,
	"int192" => Int192,
	"int200" => Int200,
	"int208" => Int208,
	"int216" => Int216,
	"int224" => Int224,
	"int232" => Int232,
	"int240" => Int240,
	"int248" => Int248,
	"int256" => Int256,
	"uint8"   => UInt8, # most common
	"uint16"  => UInt16, # most common
	"uint24"  => UInt24,
	"uint32"  => UInt32, # most common
	"uint40"  => UInt40,
	"uint48"  => UInt48,
	"uint56"  => UInt56,
	"uint64"  => UInt64, # most common
	"uint72"  => UInt72,
	"uint80"  => UInt80,
	"uint88"  => UInt88,
	"uint96"  => UInt96,
	"uint104" => UInt104,
	"uint112" => UInt112,
	"uint120" => UInt120,
	"uint128" => UInt128, # most common
	"uint136" => UInt136,
	"uint144" => UInt144, 
	"uint152" => UInt152,
	"uint160" => UInt160, # most common : address :D
	"uint168" => UInt168,
	"uint176" => UInt176,
	"uint184" => UInt184,
	"uint192" => UInt192,
	"uint200" => UInt200,
	"uint208" => UInt208,
	"uint216" => UInt216,
	"uint224" => UInt224,
	"uint232" => UInt232,
	"uint240" => UInt240,
	"uint248" => UInt248,
	"uint256" => UInt256,
	"bool" => UInt8,
	"int" => Int256,
	"uint" => UInt256,
	"address" => UInt160,
	"bytes1"  => Bytes{1}(),
	"bytes2"  => Bytes{2}(),
	"bytes3"  => Bytes{3}(),
	"bytes4"  => Bytes{4}(),
	"bytes5"  => Bytes{5}(),
	"bytes6"  => Bytes{6}(),
	"bytes7"  => Bytes{7}(),
	"bytes8"  => Bytes{8}(),
	"bytes9"  => Bytes{9}(),
	"bytes10" => Bytes{10}(),
	"bytes11" => Bytes{11}(),
	"bytes12" => Bytes{12}(),
	"bytes13" => Bytes{13}(),
	"bytes14" => Bytes{14}(),
	"bytes15" => Bytes{15}(),
	"bytes16" => Bytes{16}(),
	"bytes17" => Bytes{17}(),
	"bytes18" => Bytes{18}(),
	"bytes19" => Bytes{19}(),
	"bytes20" => Bytes{20}(),
	"bytes21" => Bytes{21}(),
	"bytes22" => Bytes{22}(),
	"bytes23" => Bytes{23}(),
	"bytes24" => Bytes{24}(),
	"bytes25" => Bytes{25}(),
	"bytes26" => Bytes{26}(),
	"bytes27" => Bytes{27}(),
	"bytes28" => Bytes{28}(),
	"bytes29" => Bytes{29}(),
	"bytes30" => Bytes{30}(),
	"bytes31" => Bytes{31}(),
	"bytes32" => Bytes{32}(),
	"bytes" => Vector{UInt},
	# "<type>[]" => Vector{<type>},
	"string" => String,
	# "function" => Bytes{24}(),
	# "fixed" => Fixed128x18,
	# "ufixed" => UFixed128x18,
)

#%%
# for: fixed<M>x<N>: signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 == 0, and 0 < N <= 80, which denotes the value v as v / (10 ** N).
# using FixedPointNumbers which is actually builtin functions AFAIK... 
# But ChatGPT for more information... it maybe can even implement the whole thing above.. :D
# struct FixedPoint{M,N} <: Real
#     data::Int
# end

# # Define the arithmetic operations
# Base.:+(a::FixedPoint{M,N}, b::FixedPoint{M,N}) where {M,N} = FixedPoint{M,N}(a.data + b.data)
# Base.:-(a::FixedPoint{M,N}, b::FixedPoint{M,N}) where {M,N} = FixedPoint{M,N}(a.data - b.data)
# Base.:*(a::FixedPoint{M,N}, b::FixedPoint{M,N}) where {M,N} = FixedPoint{M,N}(a.data * b.data ÷ (10^N))
# Base.:/(a::FixedPoint{M,N}, b::FixedPoint{M,N}) where {M,N} = FixedPoint{M,N}(a.data * (10^N) ÷ b.data)

# # Define the conversion functions
# FixedPoint{M,N}(x::Real) where {M,N} = FixedPoint{M,N}(round(Int, x * 10^N))
# Base.promote_rule(::Type{FixedPoint{M,N}}, ::Type{T}) where {M,N,T} = FixedPoint{M,N}

# # Define the bit-size constraints
# # @assert 8 ≤ M ≤ 256 && M % 8 == 0 && 0 < N ≤ 80

# # Define the actual fixed-point types
# for m = 8:8:256, n = 1:80
#     eval(Meta.parse("const fixed$(m)x$(n) = FixedPoint{$m,$n}"))
# end
# fixed24x4(1.234)

# It just didn't work... something is odd here, but no time for that....

