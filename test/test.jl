
res = getABIfile("0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2", "K4W4UAYAFWATWIA9QE8N6YG5YCWKCQE2N7")

#%%

using Boilerplate
for dat in JSON.parse(res[:result])
	@display dat
end

#%%
JSON.json(Dict([
		:jsonrpc => "2.0"
		:method => :method
		:params => []
]))

#%%
function PackedBEInt32(value::Int32)
	PackedBEInt32(reinterpret(UInt8, Ref(value), 4)...)
end
PackedBEInt32(Int32(2))

#%%

using Printf

@nbytes x = ntuple(i -> 0, 4);
@show x

# @nbytes BEInt32 = struct
# 		x::NTuple{4, UInt8} > # use '>' for big-endian byte order
# end

value = 0x12345678;

x .= reinterpret(UInt8, value);

@printf("Value: 0x%x\n", reinterpret(Int32, x))
# Value: 0x12345678	

#%%


