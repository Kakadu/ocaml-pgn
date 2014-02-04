open Ostap
open Ostap.Matcher
open Str

let identRegexp = "[a-zA-Z]\\([a-zA-Z0-9]\\)*\\b"
let tagNameRegexp = "\\([a-zA-Z0-9]\\)+\\b"
let stringLiteralRegexp = "\\([0-9a-zA-Z\\?\\.\\ \\,]\\)*"
let notBracesRegexp = "[^{}]*"

class ['ans, 'arg] lexer s =
  let skip     = Skip.create [Skip.whitespaces " \n\t\r"] in
  (*let ident    = regexp identRegexp in*)
  let tagname  = regexp tagNameRegexp in
  let literal  = regexp "[0-9]+" in
  let notBraces = regexp notBracesRegexp in
  let stringLiteral = regexp stringLiteralRegexp in

  object (self)
    inherit Matcher.t s
    method skip p coord = skip s p coord
(*
    method getIDENT     = self#get "identifier" ident *)
    method getTAGNAME   = self#get "tagname"    tagname
    method getLITERAL   = self#get "literal"    literal
    method getSTRINGLITERAL = self#get "stringLiteral" stringLiteral
    method getHORIZ     = self#get "horizontal" (Str.regexp "(1-8)")
    method getCOMMENT   = self#get "comment"    notBraces
    method getSTRINGINQUOTES =
      let ans = self#get "stringInQuotes"  (Str.regexp "\"\\([0-9a-zA-Z\\?\'\\.\\, \\/\\|]\\)*\"") in
      ans

  end

