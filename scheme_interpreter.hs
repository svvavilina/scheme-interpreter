import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Error
import System.IO
import Data.IORef

instance Show SchemeVal where show = showVal
instance Show SchemeError where show = showError
instance Error SchemeError where
	noMsg = Default "An error has occurred"
	strMsg = Default

type ThrowsError = Either SchemeError
type Env = IORef [(String, IORef SchemeVal)]
type IOThrowsError = ErrorT SchemeError IO

data Unpacker = forall a. Eq a => AnyUnpacker (SchemeVal -> ThrowsError a)

--for environment variables
nullEnv :: IO Env
nullEnv = newIORef []

--permitted symbols list
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--Scheme data types
data SchemeVal = Atom String
		| List [SchemeVal]
		| DottedList [SchemeVal] SchemeVal
		| Vector (Array Int SchemeVal)
		| Number Integer
		| Float Double
		| Ratio Rational
		| Complex (Complex Double)
		| String String
		| Bool Bool
		| Character Char
		| PrimitiveFunc ([SchemeVal] -> ThrowsError SchemeVal)
		| Func { params :: [String], vararg :: (Maybe String), body :: [SchemeVal], closure :: Env }

--to skip spaces in reading strings
spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser SchemeVal
parseAtom = do 
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of 
		"#t" -> Bool True
		"#f" -> Bool False
		_    -> Atom atom

parseList :: Parser SchemeVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser SchemeVal
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseQuoted :: Parser SchemeVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser SchemeVal
parseQuasiQuoted = do
	char '`'
	x <- parseExpr
	return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser SchemeVal
parseUnQuote = do
	char ','
	x <- parseExpr
	return $ List [Atom "unquote", x]

parseVector :: Parser SchemeVal
parseVector = do
	arrayValues <- sepBy parseExpr spaces
	return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

parseNumber :: Parser SchemeVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser SchemeVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser SchemeVal
parseDecimal2 = do
		try $ string "#d"
		x <- many1 digit
		(return . Number . read) x

parseHex :: Parser SchemeVal
parseHex = do
		try $ string "#x"
		x <- many1 hexDigit
		return $ Number (hex2dig x)

parseOct :: Parser SchemeVal
parseOct = do
		try $ string "#o"
		x <- many1 octDigit
		return $ Number (oct2dig x)

parseBin :: Parser SchemeVal
parseBin = do
		try $ string "#b"
		x <- many1 (oneOf "10")
		return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseFloat :: Parser SchemeVal
parseFloat = do
	x <- many1 digit
	char '.'
	y <- many1 digit
	return $ Float (fst.head$readFloat (x++"."++y))

parseRatio :: Parser SchemeVal
parseRatio = do
	x <- many1 digit
	char '/'
	y <- many1 digit
	return $ Ratio ((read x) % (read y))

toDouble :: SchemeVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser SchemeVal
parseComplex = do
	x <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
	char '+' 
	y <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
	char 'i' 
	return $ Complex (toDouble x :+ toDouble y)

parseString :: Parser SchemeVal
parseString = do
	char '"'
	x <- many $ escapedChars <|> noneOf "\"\\"
	char '"'
	return $ String x

escapedChars :: Parser Char
escapedChars = do
	char '\\' 
	x <- oneOf "\\\"nrt" 
	return $ case x of 
		'\\' -> x
		'"'  -> x
		'n'  -> '\n'
		'r'  -> '\r'
		't'  -> '\t'

parseBool :: Parser SchemeVal
parseBool = do
	char '#'
	(char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser SchemeVal
parseCharacter = do
	try $ string "#\\"
	value <- try (string "newline" <|> string "space") <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
	return $ Character $ case value of
		"space" -> ' '
		"newline" -> '\n'
		otherwise -> (value !! 0)

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
	<|> parseString
	<|> try parseNumber
	<|> try parseFloat
	<|> try parseRatio
	<|> try parseComplex
	<|> try parseBool
	<|> try parseCharacter
	<|> try parseQuoted
	<|> try parseQuasiQuoted
	<|> try parseUnQuote
	<|> try (do 
		string "#("
		x <- parseVector
		char ')'
		return x)
	<|> do
		char '('
		x <- try parseList <|> parseDottedList
		char ')'
		return x

--read

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readExpr :: String -> ThrowsError SchemeVal
readExpr input = case parse parseExpr "Scheme" input of
     Left err -> throwError $ Parser err
     Right val -> return val

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
	result <- prompt
	if pred result 
		then return ()
		else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalAndPrint


--start printing and evaluating

showVal :: SchemeVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = "(lambda (" ++ unwords (map show args) ++
	(case varargs of
		Nothing -> ""
		Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map showVal

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
	do
		result <- eval env pred
		case result of
			Bool False -> eval env alt
			otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
	eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
	eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
	makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
	makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
	makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
	makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
	makeVarArgs varargs env [] body
eval env (List (function : args)) = do
	func <- eval env function
	argVals <- mapM (eval env) args
	apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
	if num params /= num args && varargs == Nothing
		then throwError $ NumArgs (num params) args
		else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
	where
		remainingArgs = drop (length params) args
		num = toInteger . length
		evalBody env = liftM last $ mapM (eval env) body
		bindVarArgs arg env = case arg of
			Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
			Nothing -> return env

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

--calculation, comparison etc

primitives :: [(String, [SchemeVal] -> ThrowsError SchemeVal)]
primitives = [("+" , numericBinop (+)) ,
		("-" , numericBinop (-)) ,
		("*" , numericBinop (*)) ,
		("/" , numericBinop div) ,
		("mod" , numericBinop mod) ,
		("quotient" , numericBinop quot) ,
		("remainder" , numericBinop rem) ,
		("=", numBoolBinop (==)),
		("<", numBoolBinop (<)),
		(">", numBoolBinop (>)),
		("/=", numBoolBinop (/=)),
		(">=", numBoolBinop (>=)),
		("<=", numBoolBinop (<=)),
		("&&", boolBoolBinop (&&)),
		("||", boolBoolBinop (||)),
		("string=?", strBoolBinop (==)),
		("string<?", strBoolBinop (<)),
		("string>?", strBoolBinop (>)),
		("string<=?", strBoolBinop (<=)),
		("string>=?", strBoolBinop (>=)),
		("car", car),
		("cdr", cdr),
		("cons", cons),
		("eq?", eqv),
		("eqv?", eqv),
		("equal?", equal),
		("symbol?" , unaryOp symbolp) ,
		("string?" , unaryOp stringp) ,
		("number?" , unaryOp numberp) ,
		("bool?", unaryOp boolp) ,
		("list?" , unaryOp listp)]

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> ThrowsError SchemeVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: SchemeVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
			if null parsed 
				then throwError $ TypeMismatch "number" $ String n
				else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: SchemeVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: SchemeVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: SchemeVal -> SchemeVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
	do
		unpacked1 <- unpacker arg1
		unpacked2 <- unpacker arg2
		return $ unpacked1 == unpacked2
		`catchError` (const $ return False)

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (SchemeVal -> ThrowsError a) -> (a -> a -> Bool) -> [SchemeVal] -> ThrowsError SchemeVal
boolBinop unpacker op args = if length args /= 2 
				then throwError $ NumArgs 2 args
				else do
					left <- unpacker $ args !! 0
					right <- unpacker $ args !! 1
					return $ Bool $ left `op` right

car :: [SchemeVal] -> ThrowsError SchemeVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [SchemeVal] -> ThrowsError SchemeVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [SchemeVal] -> ThrowsError SchemeVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [SchemeVal] -> ThrowsError SchemeVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
	where eqvPair (x1, x2) = case eqv [x1, x2] of
					Left err -> False
					Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [SchemeVal] -> ThrowsError SchemeVal
equal [arg1, arg2] = do
	primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
	eqvEquals <- eqv [arg1, arg2]
	return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unaryOp :: (SchemeVal -> SchemeVal) -> [SchemeVal] -> ThrowsError SchemeVal
unaryOp f [v] = return $ f v

symbolp, numberp, stringp, boolp, listp :: SchemeVal -> SchemeVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: SchemeVal -> SchemeVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

--exceptions

data SchemeError = NumArgs Integer [SchemeVal]
		| TypeMismatch String SchemeVal
		| Parser ParseError
		| BadSpecialForm String SchemeVal
		| NotFunction String String
		| UnboundVar String String
		| Default String

--show calculating etc errors

showError :: SchemeError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--IO exceptions

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError SchemeVal
getVar envRef var  =  do
	env <- liftIO $ readIORef envRef
	maybe (throwError $ UnboundVar "Getting an unbound variable" var)
		(liftIO . readIORef)
		(lookup var env)

setVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var value = do
	env <- liftIO $ readIORef envRef
	maybe (throwError $ UnboundVar "Setting an unbound variable" var)
		(liftIO . (flip writeIORef value))
		(lookup var env)
	return value

defineVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var value = do
	alreadyDefined <- liftIO $ isBound envRef var
	if alreadyDefined
		then setVar envRef var value >> return value
		else liftIO $ do
			valueRef <- newIORef value
			env <- readIORef envRef
			writeIORef envRef ((var, valueRef) : env)
			return value

bindVars :: Env -> [(String, SchemeVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where
		extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
		addBinding (var, value) = do
					ref <- newIORef value
					return (var, ref)

main :: IO ()
main = do
	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> runOne $ args !! 0
		otherwise -> putStrLn "Program takes only 0 or 1 argument"