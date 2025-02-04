if exists("b:current_syntax")
  finish
endif


syn keyword loliKeyWorld and or struct enum fn if else for match in con var return break continue pub error import
syn keyword loliBool true false null
syn keyword loliType int i32 i16 u16 u64 uint string u8 float f32 f64
syn match loliType "\<[A-Z][a-zA-Z0-9_]*"
syn region loliString start=/"/ end=/"/
syn region loliChar start=/'/ end=/'/
syn match loliComment "#.*$"
syn match loliNumber1 "\<\d\+\(\.\d*\)\=\>"
syn match loliNumber2 "\<\d\(_\d\+\)*\(\.\d*\)\=\>"
syn match loliBuildin "@[a-zA-Z_][a-zA-Z0-9_]*"
syn region loliBlock start="{" end="}" transparent fold
syn match loliOperator display "\V\[-+/*=^&?|!><%~]"
syn match loliFunction "\zs\(\k\w*\)*\s*\ze("
syntax match ConstantName /\<[A-Z0-9_]\+\>/

hi link loliKeyWorld Keyword
hi link loliBuildin Boolean
hi link loliFunction Function
hi link loliString String
hi link loliChar String
hi link loliComment Comment
hi link loliBool Boolean
hi link loliType Type
hi link loliNumber1 Number
hi link loliNumber2 Number
hi link loliOperator Operator
hi link ConstantName Constant

let b:current_syntax = "loli"
