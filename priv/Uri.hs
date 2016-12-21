module Uri where
import Network.URI

import Zepto.Types.Export

joinDoc = unlines

joinPDoc = unlines . flip (:) parseDocSkeleton

joinParseableDoc = unlines . flip (:) parseableDocSkeleton

joinPredDoc = unlines . flip (:) predDocSkeleton

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

predDocSkeleton = [""
                  ,"params:"
                  ,"- uri: the uri to check"
                  ,"complexity: O(n)"
                  ,"returns: a boolean"
                  ]

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [("null", noIOArg nullUri, joinDoc nullDoc),
           ("parse-uri", unaryIOOp parse, joinPDoc parseDoc),
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
           ("to-string", unaryIOOp stringify, joinDoc stringifyDoc),
           ("to-unsafe-string",
            unaryIOOp stringifyUnsafe,
            joinDoc stringifyUnsafeDoc),
           ("reserved?", unaryIOOp res, joinDoc resDoc),
           ("unreserved?", unaryIOOp unres, joinDoc unresDoc),
           ("uri-is-absolute?", unaryIOOp uriIsAbs, joinPredDoc uriIsAbsDoc),
           ("uri-is-relative?", unaryIOOp uriIsRel, joinPredDoc uriIsRelDoc),
           ("relative-to", relativeT, joinDoc relTDoc),
           ("relative-from", relativeF, joinDoc relFDoc)
          ]

nullDoc = ["construct an empty URI."
          ,""
          ,"complexity: O(1)"
          ,"returns: an empty URI"
          ]

nullUri = return $ toOpaque $ nullURI

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

unpickle fun uri@(Opaque _) =
  case (fromOpaque uri) :: Maybe URI of
    Just v -> fun v
    Nothing -> lispErr $ TypeMismatch "uri" uri
unpickle _ x = lispErr $ TypeMismatch "uri" x

stringifyDoc = ["convert an URI <par>uri</par> to a string."
               ,""
               ,"params:"
               ,"- uri: the URI to stringify"
               ,"complexity: O(n)"
               ,"returns: a string"
               ]

stringify = unpickle (return . fromSimple . String . show)

stringifyUnsafeDoc = ["like <fun>uri:to-string</fun>, but does not hide passwords."
                     ,""
                     ,"params:"
                     ,"- uri: the URI to stringify"
                     ,"complexity: O(n)"
                     ,"returns: a string"
                     ]

stringifyUnsafe =
  unpickle (return . fromSimple . String . (\f -> f "") . (uriToString id))

resDoc = ["return whether <par>char</par> is a reserved character in "
         ,"a URI. To include a literal instance of one of these "
         ,"characters in a component of a URI, it must be escaped."
         ,""
         ,"params:"
         ,"- char: the character to check"
         ,"complexity: O(n)"
         ,"returns: a boolean"
         ]

res (SimpleVal (Character c)) = return $ fromSimple $ Bool $ isReserved c
res x = lispErr $ TypeMismatch "character" x

unresDoc = ["return whether <par>char</par> is a n unreserved character"
           ,"in a URI. These characters do not need to be escaped in a"
           ,"URI."
           ,""
           ,"params:"
           ,"- char: the character to check"
           ,"complexity: O(n)"
           ,"returns: a boolean"
           ]

unres (SimpleVal (Character c)) = return $ fromSimple $ Bool $ isUnreserved c
unres x = lispErr $ TypeMismatch "character" x

uriIsAbsDoc = "check whether <par>uri</par> is absolute."

uriIsAbs = unpickle (return . fromSimple . Bool . uriIsAbsolute)

uriIsRelDoc = "check whether <par>uri</par> is relative."

uriIsRel = unpickle (return . fromSimple . Bool . uriIsRelative)

concat' fun [first@(Opaque _), second@(Opaque _)] =
  case (fromOpaque first) :: Maybe URI of
    Just v ->
      case (fromOpaque second) :: Maybe URI of
        Just v2 -> return $ toOpaque $ fun v v2
        Nothing -> lispErr $ TypeMismatch "uri" second
    Nothing -> lispErr $ TypeMismatch "uri" first
concat' _ [x, (Opaque _)] = lispErr $ TypeMismatch "uri" x
concat' _ [_, x] = lispErr $ TypeMismatch "uri" x
concat' _ x = lispErr $ NumArgs 2 x

relTDoc = ["concatenate two URIs, making the <par>first</par>"
          ,"relative to the <par>second</par>."
          ,""
          ,"params:"
          ,"- first: the first URI (will be the absolute)"
          ,"- second: the second URI (will be the reference)"
          ,"complexity: O(1)"
          ,"returns: a URI"
          ]

relativeT = concat' relativeTo

relFDoc = ["return a new URI which represents the relative location"
          ,"of the <par>first</par> one with respect to the <par>second</par>"
          ,"one. Thus, the values supplied are expected to be absolute URIs,"
          ,"and the result returned may be a relative URI."
          ,""
          ,"params:"
          ,"- first: the first URI (will be the absolute)"
          ,"- second: the second URI (will be the reference)"
          ,"complexity: O(1)"
          ,"returns: a URI"
          ]

relativeF = concat' relativeFrom
