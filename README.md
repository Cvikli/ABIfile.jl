# ABIfile.jl
Application Binary Interface (ABI) for solidity smart contract in the Ethereum ecosystem

For me this is more understandable and modular implementation.

Let's get to know EVERYTHING from the Crypto world! ;)

# Functions
getABI(contract_address::String, etherscan_api_key::String): Get ABI file from contract address
readABI(contractname::String, stream::IO): Read an ABI file for a contract
encodefunctioncall(io::IO, f::ABIFunction, inputs::Array): Encode a call to a function
encodefunctioncall(io::IOBuffer, f::ABIFunction, inputs::Array): Encode a call to a function, returns buffer data
encodefunctionresult(io::IO, f::ABIFunction, outputs::Array): Encode the results of a function
encodefunctionresult(io::IOBuffer, f::ABIFunction, outputs::Array): Encode the results of a function, returns buffer data
encodeevent(io::IO, e::ABIEvent, inputs::Array): Encode an event
encodeevent(io::IOBuffer, e::ABIEvent, inputs::Array): Encode an event, returns buffer data
decodefunctioncall(io::IO, con::Contract): Decode a function call
decodefunctionresult(io::IO, con::Contract): Decode a function call result
decodeevent(io::IO, con::Contract): Decode an event in a transaction log

This is a modification of a very similar ABI parser of Web3.jl: https://github.com/lambda-mechanics/Web3.jl
But there are serious amount of changes and also it is exported to a module and it doesn't depend from the Web3 protocol at all. 


# Contribute

Please if you find or notice any problem don't hesitate to improve the library. :)

