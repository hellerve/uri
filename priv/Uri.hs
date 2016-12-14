module Uri where
import Network.URI

import Zepto.Types.Export

joinDoc = unlines

joinPDoc = unlines . flip (:) parseDocSkeleton

joinParseableDoc = unlines . flip (:) parseableDocSkeleton

parseDocSkeleton = ["If the parse fails, false is returned."
                   ,""
                   ,"params:"
                   ,"- str: the string to parse"
                   ,"complexity: O(n)"
                   ,"returns: a URI type or false"
                   ]

parseableDocSkeleton = [""
                       ,"params:"
                       ,"- str: the string to check for parseability"
                       ,"complexity: O(n)"
                       ,"returns: a boolean"
                       ]

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [("parse-uri", unaryIOOp parse, joinPDoc parseDoc),
           ("parse-uri-reference", unaryIOOp parseRef, joinPDoc parseRefDoc),
           ("parse-uri-relative-reference",
            unaryIOOp parseRelativeRef,
            joinPDoc parseRelativeRefDoc),
           ("parse-absolute-uri",
            unaryIOOp parseAbsolute,
            joinPDoc parseAbsoluteDoc),
           ("uri?", unaryIOOp check, joinDoc checkDoc),
           ("parseable?", unaryIOOp parseable, joinParseableDoc parseableDoc),
           ("is-reference?", unaryIOOp isRef, joinParseableDoc isRefDoc),
           ("is-relative-reference?",
            unaryIOOp isRelRef,
            joinParseableDoc isRelRefDoc),
           ("is-absolute?", unaryIOOp isAbs, joinParseableDoc isAbsDoc),
           ("is-v4?", unaryIOOp isV4, joinParseableDoc isV4Doc),
           ("is-v6?", unaryIOOp isV6, joinParseableDoc isV6Doc),
           ("to-string", unaryIOOp stringify, joinDoc stringifyDoc)]

parse' parsef (SimpleVal (String s)) = case parsef s of
  Just uri -> return $ toOpaque uri
  Nothing -> return $ fromSimple $ Bool False
parse' _ x = lispErr $ TypeMismatch "string" x

parseDoc = "parse a string into a URI type."

parse = parse' parseURI

parseRefDoc = "parse a string into a URI type (assume it is a reference)."

parseRef = parse' parseURIReference

parseRelativeRefDoc =
    "parse a string into a URI type (assume it is a relative reference)."

parseRelativeRef = parse' parseRelativeReference

parseAbsoluteDoc = "parse a string into a URI type (assume it is absolute)."

parseAbsolute = parse' parseAbsoluteURI

checkDoc = ["check whether <par>obj</par> is an URI type."
           ,""
           ,"params:"
           ,"- obj: the element to check"
           ,"complexity: O(1)"
           ,"returns: a boolean"
           ]

check uri@(Opaque _) = case (fromOpaque uri) :: Maybe URI of
  Just _ -> return $ fromSimple $ Bool True
  Nothing -> return $ fromSimple $ Bool False
check _ = return $ fromSimple $ Bool False

parseable' pred (SimpleVal (String s)) = return $ fromSimple $ Bool $ pred s
parseable' _ x = lispErr $ TypeMismatch "string" x

parseableDoc = "check whether <par>str</par> can be parsed into a URI."

parseable = parseable' isURI

isRefDoc = "check whether <par>str</par> can be parsed into a reference."

isRef = parseable' isURIReference

isRelRefDoc = "check whether <par>str</par> can be parsed into a relative reference."

isRelRef = parseable' isRelativeReference

isAbsDoc = "check whether <par>str</par> can be parsed into an absolute URI."

isAbs = parseable' isAbsoluteURI

isV4Doc = "check whether <par>str</par> is a valid IPv4 address."

isV4 = parseable' isIPv4address

isV6Doc = "check whether <par>str</par> is a valid IPv6 address."

isV6 = parseable' isIPv6address

stringifyDoc = ["convert an URI <par>uri</par> to a string."
               ,""
               ,"params:"
               ,"- uri: the URI to stringify"
               ,"complexity: O(n)"
               ,"returns: a string"
               ]

stringify uri@(Opaque _) = case (fromOpaque uri) :: Maybe URI of
  Just v -> return $ fromSimple $ String $ show v
  Nothing -> lispErr $ TypeMismatch "string" uri
stringify x = lispErr $ TypeMismatch "uri" x
