type tokenData = | TokenData (string, int, int);

type constantLiteral =
  | Null tokenData
  | Int (tokenData, int)
  | Float (tokenData, float)
  | StringSQ (tokenData, string);

type simpleType =
  | TypeBool tokenData | TypeFloat tokenData | TypeInt tokenData | TypeString tokenData;

type identifier = | Identifier (tokenData, string);

type variable = | This tokenData | Variable (tokenData, string);

type identifier_variable = | IdentVar_ident identifier | IdentVar_var variable;

type expression =
  | ConstLiteral constantLiteral
  | StringDQ (tokenData, string)
  | VarExpr variable
  | IdentExpr identifier
  | New (identifier, list expression)
  | Dereference (expression, list identifier_variable) /* (f()[5])->$a->b->$c */
  | Parent (list identifier_variable)
  | Self (list identifier_variable)
  | StaticReferenceChain (identifier, list identifier_variable) /* A::$a->b->$c */
  | ArrayExpr (expression, expression)
  | FunCallExpr (expression, list expression)
  | PreDecrement expression
  | PreIncrement expression
  | PostDecrement expression
  | PostIncrement expression
  | UnaryMinus expression
  | BitwiseNot expression
  | TypeCast (simpleType, expression)
  | InstanceOf (expression, identifier)
  | LogicalNot expression
  | Multiplication (expression, expression)
  | Division (expression, expression)
  | Modulo (expression, expression)
  | Plus (expression, expression)
  | Minus (expression, expression)
  | Concat (expression, expression)
  | ShiftLeft (expression, expression)
  | ShiftRight (expression, expression)
  | IsSmaller (expression, expression)
  | IsSmallerEq (expression, expression)
  | IsEqual (expression, expression)
  | IsIdentical (expression, expression)
  | BitwiseAnd (expression, expression)
  | BitwiseXor (expression, expression)
  | BitwiseOr (expression, expression)
  | LogicalAnd (expression, expression)
  | LogicalXor (expression, expression)
  | LogicalOr (expression, expression)
  | TernaryChoice (expression, expression, expression)
  | AssignExpr (expression, expression);

type formalArgument = | FormalArg variable | TypedFormalArg (identifier, variable);

type formalArgumentWDefault = | FormalArgWDefault (formalArgument, constantLiteral);

type formalArgsList = (list formalArgument, list formalArgumentWDefault);

type switchItem = | SwCase expression | SwDefault;

type variableDeclaration = | VarDecl variable | VarDeclAssig (variable, expression);

type statement =
  | AssignStmt (expression, expression)
  | VarDeclStmt variableDeclaration
  | FunCallStmt (expression, list expression)
  | Break tokenData
  | Return expression
  | Throw expression
  | If (expression, statement)
  | IfElse (expression, statement, statement)
  | SwitchStmt (expression, list (switchItem, statement))
  | TryCatch (statement, list (formalArgument, statement)) /* Must be >=1 catch clauses! */
  | BlockStmt (list statement);

type functionDefinition = | Function (identifier, formalArgsList, statement);

type classItemVisibility = | Public | Protected | Private;

/* TODO Add instanceVar */
type classItem =
  | InstanceMethod (classItemVisibility, functionDefinition)
  | StaticMethod (classItemVisibility, functionDefinition)
  | StaticVar (classItemVisibility, variableDeclaration)
  | StaticConst (classItemVisibility, identifier, expression);

type abstractClause = | Concrete | Abstract;

type extendsClause = | RootClass | Extends identifier;

type classDefinition = | Class (abstractClause, identifier, extendsClause, list classItem);

type sourceFileItem =
  | PHPStatement statement | PHPFunction functionDefinition | PHPClass classDefinition;

type sourceFile = | PHPSourceFile (list sourceFileItem);

let funCallExprAsPair: expression => (expression, list expression);
