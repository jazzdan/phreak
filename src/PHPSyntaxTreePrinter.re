/*
 * vim: set ft=reason:
 */
open PHPSyntaxTree;

let listToString lfun ll sepa =>
  List.fold_left
    (
      fun s item =>
        (
          if (1 <= String.length s) {
            s ^ sepa
          } else {
            ""
          }
        ) ^
          lfun item
    )
    ""
    ll;

let toString_simpleType st =>
  switch st {
  | TypeBool _ => "bool"
  | TypeFloat _ => "float"
  | TypeInt _ => "int"
  | TypeString _ => "string"
  };

let toString_constantLiteral cl =>
  switch cl {
  | Null _ => "Null"
  | Int _ i [@implicit_arity] => string_of_int i
  | Float _ f [@implicit_arity] => string_of_float f
  | StringSQ _ s [@implicit_arity] => "'" ^ s ^ "'"
  };

let toString_identifier i =>
  switch i {
  | Identifier _ s [@implicit_arity] => s
  };

let toString_variable v =>
  switch v {
  | This _ => "$this"
  | Variable _ s [@implicit_arity] => "$" ^ s
  };

let toString_formalArgument fa =>
  switch fa {
  | FormalArg v => toString_variable v
  | TypedFormalArg i v [@implicit_arity] => toString_identifier i ^ " " ^ toString_variable v
  };

let toString_formalArgumentDefault (FormalArgWDefault fa clit [@implicit_arity]) =>
  toString_formalArgument fa ^ " = " ^ toString_constantLiteral clit;

let toString_formalArgument_list falist => listToString toString_formalArgument falist ", ";

let toString_formalArgumentDefault_list fawdlist =>
  listToString toString_formalArgumentDefault fawdlist ", ";

let toString_formalArgs_list2 (falist, fawdlist) => {
  let falistStr = toString_formalArgument_list falist
  and fawdlistStr = toString_formalArgumentDefault_list fawdlist;
  let sepa =
    if (1 <= String.length falistStr && 1 <= String.length fawdlistStr) {
      ", "
    } else {
      ""
    };
  falistStr ^ sepa ^ fawdlistStr
};

let toString_identifier_variable iv =>
  switch iv {
  | IdentVar_ident i => toString_identifier i
  | IdentVar_var v => toString_variable v
  };

let toString_identifier_variable_list ivlist =>
  listToString toString_identifier_variable ivlist "->";

/* comma separated expression list */
let toString_expression_list toString_expr el => listToString toString_expr el ", ";


/**
 * Pretty printing: we try to use as few () as possible.
 */
let toString_expression expr => {
  let rec exprStr precReq expr => {
    let par precActual s =>
      if (precActual < precReq) {
        "(" ^ s ^ ")"
      } else {
        s
      };
    let unOpStrPre e ops precActual isRightAssoc => {
      let precReqRight = precActual + (if isRightAssoc {0} else {1});
      par precActual ops ^ exprStr precReqRight e
    }
    and unOpStrPost e ops precActual isLeftAssoc => {
      let precReqLeft = precActual + (if isLeftAssoc {0} else {1});
      par precActual (exprStr precReqLeft e) ^ ops
    }
    and binOpStr e1 e2 ops precActual isLeftAssoc isRightAssoc => {
      let precReqLeft = precActual + (if isLeftAssoc {0} else {1})
      and precReqRight = precActual + (if isRightAssoc {0} else {1});
      par precActual (exprStr precReqLeft e1 ^ " " ^ ops ^ " " ^ exprStr precReqRight e2)
    };
    switch expr {
    | ConstLiteral c => toString_constantLiteral c
    | StringDQ _ s [@implicit_arity] => "\"" ^ s ^ "\""
    | VarExpr v => toString_variable v
    | IdentExpr i => toString_identifier i
    | New i el [@implicit_arity] =>
      par 24 ("new " ^ toString_identifier i ^ "(" ^ toString_expression_list (exprStr 0) el ^ ")")
    | Dereference e [] [@implicit_arity] => par 23 (exprStr precReq e)
    | Dereference e [h, ...t] [@implicit_arity] =>
      par 23 (exprStr 23 e ^ "->" ^ toString_identifier_variable_list [h, ...t])
    | Parent ivlist => par 22 ("parent::" ^ toString_identifier_variable_list ivlist)
    | Self ivlist => par 22 ("self::" ^ toString_identifier_variable_list ivlist)
    | StaticReferenceChain i [] [@implicit_arity] => toString_identifier i
    | StaticReferenceChain i ivlist [@implicit_arity] =>
      par 22 (toString_identifier i ^ "::" ^ toString_identifier_variable_list ivlist)
    | ArrayExpr e1 e2 [@implicit_arity] => par 21 (exprStr 21 e1 ^ "[" ^ exprStr 0 e2 ^ "]")
    | FunCallExpr f al [@implicit_arity] =>
      par 20 (exprStr 21 f ^ "(" ^ toString_expression_list (exprStr 0) al ^ ")")
    | PreDecrement e => unOpStrPre e "--" 19 false
    | PreIncrement e => unOpStrPre e "++" 19 false
    | PostDecrement e => unOpStrPost e "--" 19 false
    | PostIncrement e => unOpStrPost e "++" 19 false
    | UnaryMinus e => unOpStrPre e "-" 18 false
    | BitwiseNot e => unOpStrPre e "~" 18 false
    | TypeCast s e [@implicit_arity] => par 18 ("(" ^ toString_simpleType s ^ ") " ^ exprStr 19 e)
    | InstanceOf e i [@implicit_arity] =>
      par 17 (exprStr 18 e ^ " instanceof " ^ toString_identifier i)
    | LogicalNot e => unOpStrPre e "!" 16 true
    | Multiplication e1 e2 [@implicit_arity] => binOpStr e1 e2 "*" 15 true true
    | Division e1 e2 [@implicit_arity] => binOpStr e1 e2 "/" 15 true false
    | Modulo e1 e2 [@implicit_arity] => binOpStr e1 e2 "%" 15 true false
    | Plus e1 e2 [@implicit_arity] => binOpStr e1 e2 "+" 14 true true
    | Minus e1 e2 [@implicit_arity] => binOpStr e1 e2 "-" 14 true false
    | Concat e1 e2 [@implicit_arity] => binOpStr e1 e2 "." 14 true true
    | ShiftLeft e1 e2 [@implicit_arity] => binOpStr e1 e2 "<<" 13 true false
    | ShiftRight e1 e2 [@implicit_arity] => binOpStr e1 e2 ">>" 13 true false
    | IsSmaller e1 e2 [@implicit_arity] => binOpStr e1 e2 "<" 12 false false
    | IsSmallerEq e1 e2 [@implicit_arity] => binOpStr e1 e2 "<=" 12 false false
    | IsEqual e1 e2 [@implicit_arity] => binOpStr e1 e2 "==" 11 false false
    | IsIdentical e1 e2 [@implicit_arity] => binOpStr e1 e2 "===" 11 false false
    | BitwiseAnd e1 e2 [@implicit_arity] => binOpStr e1 e2 "&" 10 true true
    | BitwiseXor e1 e2 [@implicit_arity] => binOpStr e1 e2 "^" 9 true true
    | BitwiseOr e1 e2 [@implicit_arity] => binOpStr e1 e2 "|" 8 true true
    | LogicalAnd e1 e2 [@implicit_arity] => binOpStr e1 e2 "&&" 7 true true
    | LogicalOr e1 e2 [@implicit_arity] => binOpStr e1 e2 "||" 6 true true
    | TernaryChoice e1 e2 e3 [@implicit_arity] =>
      par 5 (exprStr 6 e1 ^ " ? " ^ exprStr 6 e2 ^ " : " ^ exprStr 6 e3)
    | AssignExpr e1 e2 [@implicit_arity] => par 4 (exprStr 5 e1 ^ " = " ^ exprStr 5 e2)
    | LogicalXor e1 e2 [@implicit_arity] => binOpStr e1 e2 "xor" 2 true true
    }
  };
  exprStr 0 expr
};

let toString_expression_list = toString_expression_list toString_expression;

let toString_variableDeclaration vd =>
  switch vd {
  | VarDecl v => toString_variable v ^ ";" /* TODO maybe don't print the ; here */
  | VarDeclAssig v e [@implicit_arity] => toString_variable v ^ " = " ^ toString_expression e
  };

let toString_catch_list toString_stmt fasl iakk i =>
  listToString
    (
      fun
      | (fa, s) => "catch (" ^ toString_formalArgument fa ^ ") " ^ toString_stmt s (iakk ^ i) i
    )
    fasl
    "";

let toString_switchItem_list toString_stmt sisl iakk i => {
  let itemString (c, s) => {
    let stmtStr = toString_stmt s (iakk ^ i) i;
    switch c {
    | SwCase e => iakk ^ "case (" ^ toString_expression e ^ "):\n" ^ stmtStr
    | SwDefault => iakk ^ "default:\n" ^ stmtStr
    }
  };
  listToString itemString sisl ""
};

let toString_statement_list toString_stmt sl iakk i =>
  listToString
    (
      fun
      | s => toString_stmt s iakk i
    )
    sl
    "";

let rec toString_statement stmt iakk i =>
  switch stmt {
  | AssignStmt e1 e2 [@implicit_arity] =>
    iakk ^ toString_expression e1 ^ " = " ^ toString_expression e2 ^ ";\n"
  | VarDeclStmt vd => iakk ^ toString_variableDeclaration vd ^ ";\n"
  | FunCallStmt f al [@implicit_arity] =>
    iakk ^ toString_expression f ^ "(" ^ toString_expression_list al ^ ");\n"
  | Break _ => "break;"
  | Return e => iakk ^ "return " ^ toString_expression e ^ ";\n"
  | Throw e => iakk ^ "throw " ^ toString_expression e ^ ";\n"
  | If e s [@implicit_arity] =>
    iakk ^ "if (" ^ toString_expression e ^ ")\n" ^ toString_statement s (iakk ^ i) i
  | IfElse e s1 s2 [@implicit_arity] =>
    iakk ^
      "if (" ^
      toString_expression e ^
      ")\n" ^
      toString_statement s1 (iakk ^ i) i ^
      iakk ^
      "else\n" ^
      toString_statement s2 (iakk ^ i) i
  | SwitchStmt e sisl [@implicit_arity] =>
    iakk ^
      "switch (" ^
      toString_expression e ^
      ") {\n" ^
      toString_switchItem_list toString_statement sisl (iakk ^ i) i ^
      "}\n"
  | TryCatch s fasl [@implicit_arity] =>
    iakk ^
      "try {\n" ^
      toString_statement s (iakk ^ i) i ^
      toString_catch_list toString_statement fasl (iakk ^ i) i ^
      "\n"
  | BlockStmt sl =>
    iakk ^ "{\n" ^ toString_statement_list toString_statement sl (iakk ^ i) i ^ iakk ^ "}\n"
  };

let toString_statement_list = toString_statement_list toString_statement;

let toString_function_basic (Function id falist stmt [@implicit_arity]) indStart indAkk ind =>
  indStart ^
    "function " ^
    toString_identifier id ^
    "(" ^
    toString_formalArgs_list2 falist ^
    ")\n" ^
    toString_statement stmt indAkk ind;

let toString_function f indAkk ind => toString_function_basic f indAkk indAkk ind;

let toString_class (Class abstractDef classId extendsDef items [@implicit_arity]) indAkk ind => {
  let toString_classItem_list items indAkk ind => {
    let visStr vis =>
      switch vis {
      | Public => "public"
      | Protected => "protected"
      | Private => "private"
      };
    let itemStr it =>
      switch it {
      | InstanceVar vis varDecl [@implicit_arity] => 
        indAkk ^ visStr vis ^ " " ^ toString_variableDeclaration varDecl
      | InstanceMethod vis funDef [@implicit_arity] =>
        indAkk ^ visStr vis ^ " " ^ toString_function_basic funDef "" indAkk ind
      | StaticMethod vis funDef [@implicit_arity] =>
        indAkk ^ visStr vis ^ " static " ^ toString_function_basic funDef "" indAkk ind
      | StaticVar vis varDecl [@implicit_arity] =>
        indAkk ^ visStr vis ^ " static " ^ toString_variableDeclaration varDecl
      | StaticConst vis constId constExpr [@implicit_arity] =>
        indAkk ^
          visStr vis ^
          " const " ^
          toString_identifier constId ^
          " = " ^
          toString_expression constExpr
      };
    List.fold_left (fun s item => s ^ itemStr item ^ "\n") "" items
  };
  let abstractStr =
    switch abstractDef {
    | Abstract => "abstract "
    | Concrete => ""
    }
  and extendsStr =
    switch extendsDef {
    | RootClass => ""
    | Extends ie => " extends " ^ toString_identifier ie
    };
  indAkk ^
    abstractStr ^
    "class " ^
    toString_identifier classId ^
    extendsStr ^
    "\n" ^
    indAkk ^
    "{\n" ^
    toString_classItem_list items (indAkk ^ ind) ind ^
    indAkk ^
    "}\n"
};

let toString_PHPSourceFile (PHPSourceFile items) => {
  let phpItemStr indAkk ind it =>
    switch it {
    | PHPStatement stmt => toString_statement stmt indAkk ind
    | PHPFunction fund => toString_function fund indAkk ind ^ "\n"
    | PHPClass clsd => toString_class clsd indAkk ind ^ "\n"
    };
  listToString (phpItemStr "" "    ") items ""
};
