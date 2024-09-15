module clangast;
import std::io;
import std::os;
import std::core::dstring;
import std::core::array;
import std::encoding::json;
import std::collections::map;
import std::collections::list;
import std::collections::maybe;
import std::collections::object;

fault AstError 
{
    NO_CLANG,
    CLANG_ERROR,
    UNKNOWN_ENUM_VALUE,
    INVALID_AST,
}

macro parse_enum($Enum, str) @private
{
    int i = 0;
    foreach (m : $Enum.names)
    { 
        if (str.len == m.len)
        {
            bool match = true;
            usz strIdx = 0;
            usz mIdx = 0;
            while (strIdx < str.len && mIdx < m.len)
            {
                while (m[mIdx] == '_')
                {
                    mIdx ++;
                }
                if (mIdx >= m.len)
                {
                    break;
                }
                char mc = m[mIdx];
                if (mc.is_lower() @pure)
                {
                    mc -= 'a' - 'A';
                }
                if (mc != str[strIdx])
                {
                    match = false;
                    break;
                }
                mIdx ++;
                strIdx ++;
            }
            if (match)
            {
                return ($Enum) i;
            }
        }

        i ++;
    }

    return AstError.UNKNOWN_ENUM_VALUE?;
}

struct Loc 
{
    uint offset;
    uint col; 
    uint tokLen;
}

struct Range 
{
    Loc begin;
    Loc end;
}

fn Loc! parse_loc(Object* loc) @private
{
    Loc r;
    r.offset = loc.get_uint("offset")!;
    r.col = loc.get_uint("col")!;
    r.tokLen = loc.get_uint("tokLen")!;
    return r;
}

fn Range! parse_range(Object* range) @private
{
    Range r;
    r.begin = parse_loc(range.get("begin")!)!;
    r.end = parse_loc(range.get("end")!)!;
    return r;
}

struct Type 
{
    dstring::DString qual_type;
}

fn Type! parse_type(Object* type) @private
{
    Type t;
    t.qual_type = dstring::new(type.get_string("qualType")!);
    return t;
}

fn void Type.free(&self)
{
    self.qual_type.free();
}

distinct Id = uint;

fn uint Id.hash(self)
{
    return (uint) self;
}

fn Id! parse_id(String str) @private
{
    return (Id) (str[2 ..].to_uint(16)!);
}

enum NodeKind : int 
{
    TYPE_DEF,
    POINTER_TYPE,
    CONSTANT_ARRAY_TYPE,
    RECORD_TYPE,
    CLASS_TEMPLATE_DECL,
    FIELD_DECL,
    ACCESS_SPEC_DECL,
    CXX_CONSTRUCTOR_DECL,
    PARM_VAR_DECL,
    COMPOUND_STATEMENT,
    BINARY_OPERATOR,
    MEMBER_EXPR,
    CXX_THIS_EXPR,
    IMPLICIT_CAST_EXPR,
    DECL_REF_EXPR,
    CXX_METHOD_DECL,
    RETURN_STMT,
    CXX_RECORD_DECL,
    TEMPLATE_TYPE_PARM_DECL,
    NAMESPACE_DECL,
    EMPTY_DECL,
}

struct NodeConstantArrayType
{
    uint length;
}

struct NodeRecordType 
{
    Id decl_id;
}

struct NodeFieldDecl
{
    bool is_referenced;
}

enum AccessLevel
{
    PUBLIC,
    PROTECTED,
    PRIVATE,
}

fn AccessLevel! parse_access_level(String str) @private
{
    return parse_enum(AccessLevel, str);
}

struct NodeAccessSpecDecl 
{
    AccessLevel access;
}

struct NodeCXXConstructorDecl
{
    DString mangled_name;
}

struct NodeParmVarDecl 
{
    bool is_used;
}

enum ValueCategory
{
    L_VALUE,
    R_VALUE,
    P_VALUE,
    PR_VALUE,
}

fn ValueCategory! parse_value_category(String str) @private
{
    return parse_enum(ValueCategory, str);
}

struct NodeBinaryOperation
{
    ValueCategory val_category;
    DString opcode;
}

struct NodeCXXMethodDecl
{
    DString mangled_name;
}

struct NodeMemberExpr
{
    ValueCategory val_category;
    bool is_arrow;
    Id referenced_member_decl;
}

struct NodeCXXThisExpr
{
    ValueCategory val_category;
}

enum CastKind 
{
    LVALUE_TO_RVALUE,
}

fn CastKind! parse_cast_kind(String str) @private
{
    return parse_enum(CastKind, str)!;
}

struct NodeImplicitCastExpr
{
    ValueCategory val_category;
    CastKind cast_kind;
}

struct NodeDeclRefExpr
{
    ValueCategory val_category;
    Id referenced_decl;
}

enum Tag 
{
    CLASS,
    STRUCT,
    TYPENAME,
}

fn Tag! parse_tag(String str) @private 
{
    return parse_enum(Tag, str)!;
}

struct CXXRecordDeclInfo
{
    Maybe(<bool>) has_const_param;
    Maybe(<bool>) implicit_has_const_param;
    bool irrelevant;
    bool needs_implicit;
    bool simple;
    bool trivial;
}

fn void! CXXRecordDeclInfo.parse(&self, Object* obj) @private 
{
    self.has_const_param = maybe::EMPTY(<bool>);
    if (try x = obj.get_bool("hasConstParam"))
    {
        self.has_const_param = maybe::value(<bool>)(x);
    }

    self.implicit_has_const_param = maybe::EMPTY(<bool>);
    if (try x = obj.get_bool("implicitHasConstParam"))
    {
        self.implicit_has_const_param = maybe::value(<bool>)(x);
    }

    self.irrelevant = false;
    if (try x = obj.get_bool("irrelevant"))
    {
        self.irrelevant = x;
    }

    self.needs_implicit = obj.get_bool("needsImplicit")!;
    self.simple = obj.get_bool("simple")!;
    self.trivial = obj.get_bool("trivial")!;
}

struct NodeCXXRecordDecl
{
    Tag tag_used;
    bool complete_definition;
    bool can_const_default_init;
    bool can_pass_in_registers;
    Maybe(<CXXRecordDeclInfo>) copy_assign;
    Maybe(<CXXRecordDeclInfo>) copy_ctor;
    Maybe(<CXXRecordDeclInfo>) default_ctor;
    Maybe(<CXXRecordDeclInfo>) dtor;
    bool is_aggregate;
    bool has_user_declared_constructor;
    bool is_standard_layout;
    bool is_trivial;
    bool is_trivially_copyable;
    Maybe(<CXXRecordDeclInfo>) move_assign;
    Maybe(<CXXRecordDeclInfo>) move_ctor;
}

fn bool! get_bool_or_false(Object* obj, String name) @private 
{
    if (obj.has_key(name)) {
        return obj.get_bool(name)!;
    }
    return false;
}

fn void! NodeCXXRecordDecl.parse(&self, Object* obj) @private 
{
    self.tag_used = parse_tag(obj.get_string("tagUsed")!)!;
    self.complete_definition = get_bool_or_false(obj, "completeDefinition")!;
    self.can_pass_in_registers = get_bool_or_false(obj, "canPassInRegisters")!;

    self.copy_assign = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("copyAssign")!, x.has_key("simple"))
    {
        self.copy_assign.has_value = true;
        self.copy_assign.value.parse(x)!;
    }

    self.copy_ctor = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("copyCtor")!, x.has_key("simple"))
    {
        self.copy_ctor.has_value = true;
        self.copy_ctor.value.parse(x)!;
    }

    self.default_ctor = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("defaultCtor")!, x.has_key("simple"))
    {
        self.default_ctor.has_value = true;
        self.default_ctor.value.parse(x)!;
    }

    self.dtor = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("dtor")!, x.has_key("simple"))
    {
        self.dtor.has_value = true;
        self.dtor.value.parse(x)!;
    }

    self.is_aggregate = get_bool_or_false(obj, "isAggregate")!;
    self.has_user_declared_constructor = get_bool_or_false(obj, "hasUserDeclaredConstructor")!;
    self.is_standard_layout = get_bool_or_false(obj, "isStandardLayout")!;
    self.is_trivial = get_bool_or_false(obj, "isTrivial")!;
    self.is_trivially_copyable = get_bool_or_false(obj, "isTriviallyCopyable")!;

    self.move_assign = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("moveAssign")!, x.has_key("simple"))
    {
        self.move_assign.has_value = true;
        self.move_assign.value.parse(x)!;
    }

    self.move_ctor = maybe::EMPTY(<CXXRecordDeclInfo>);
    if (Object* x = obj.get("moveCtor")!, x.has_key("simple"))
    {
        self.move_ctor.has_value = true;
        self.move_ctor.value.parse(x)!;
    }
}

struct NodeTemplateTypeParmDecl
{
    bool is_referenced;
    Tag tag_used;
    uint depth;
    uint index;
}

struct Node  
{
    Id id;
    NodeKind kind;
    Maybe(<Range>) range;
    bool is_implicit;
    Maybe(<DString>) name;
    Maybe(<Type>) type;
    List(<Node>) children;
    union
    {
        NodeConstantArrayType constant_array_type;
        NodeRecordType record_type;
        NodeFieldDecl field_decl;
        NodeAccessSpecDecl access_spec_decl;
        NodeCXXConstructorDecl cxx_constructor_decl;
        NodeParmVarDecl parm_var_decl;
        NodeBinaryOperation binary_operation;
        NodeCXXMethodDecl cxx_method_decl;
        NodeMemberExpr member_expr;
        NodeCXXThisExpr cxx_this_expr;
        NodeImplicitCastExpr implicit_cast_expr;
        NodeDeclRefExpr decl_ref_expr;
        NodeCXXRecordDecl cxx_record_decl;
        NodeTemplateTypeParmDecl template_type_parm_decl;
    }
}

fn void Node.free(&self)
{
    if (self.name.has_value)
    {
        self.name.value.free();
    }
    
    foreach (node : self.children)
    {
        node.free();
    }
    self.children.free();
    
    if (self.type.has_value)
    {
        self.type.value.free();
    }

    switch (self.kind)
    {
        case NodeKind.CXX_CONSTRUCTOR_DECL:
            self.cxx_constructor_decl.mangled_name.free();

        case NodeKind.BINARY_OPERATOR:
            self.binary_operation.opcode.free();

        case NodeKind.CXX_METHOD_DECL:
            self.cxx_method_decl.mangled_name.free();

        default:
    }
}

fn NodeKind! parse_kind(String str) @private 
{
    return parse_enum(NodeKind, str);
}

fn Node! parse_node(Object* obj) @private
{
    Node n;
    
    n.id = parse_id(obj.get_string("id")!)!;

    n.kind = parse_kind(obj.get_string("kind")!)!;

    n.range = maybe::EMPTY(<Range>);
    if (try range = parse_range(obj.get("range")!))
    {
        n.range = maybe::value(<Range>)(range);
    }
    
    n.is_implicit = false;
    if (try is_implicit = obj.get_bool("isImplicit")) 
    {
        n.is_implicit = is_implicit;
    }
    else if (try is_implicit = obj.get_bool("implicit"))
    {
        n.is_implicit = is_implicit;
    }

    n.name = maybe::EMPTY(<DString>);
    if (try name = obj.get_string("name"))
    {
        n.name = maybe::value(<DString>)(dstring::new(name));
    }

    n.type = maybe::EMPTY(<Type>);
    if (try type = obj.get("type"))
    {
        n.type = maybe::value(<Type>)(parse_type(type)!);
    }

    n.children.new_init();
    if (try inner = obj.get("inner"))
    {
        if (!inner.is_indexable())
        {
            return AstError.INVALID_AST?;
        }

        foreach (x : inner.array)
        {
            n.children.push(parse_node(x)!);
        }
    }

    switch (n.kind)
    {
        case NodeKind.CONSTANT_ARRAY_TYPE:
            n.constant_array_type.length = obj.get_uint("size")!;

        case NodeKind.RECORD_TYPE:
            n.record_type.decl_id = parse_id(obj.get("decl")!.get_string("id")!)!;

        case NodeKind.FIELD_DECL:
            n.field_decl.is_referenced = obj.get_bool("isReferenced")!;

        case NodeKind.ACCESS_SPEC_DECL:
            n.access_spec_decl.access = parse_access_level(obj.get_string("access")!)!;

        case NodeKind.CXX_CONSTRUCTOR_DECL:
            n.cxx_constructor_decl.mangled_name = dstring::new(obj.get_string("mangledName")!);

        case NodeKind.PARM_VAR_DECL:
            n.parm_var_decl.is_used = obj.get_bool("isUsed")!;

        case NodeKind.BINARY_OPERATOR:
            n.binary_operation.val_category = parse_value_category(obj.get_string("valueCategory")!)!;
            n.binary_operation.opcode = dstring::new(obj.get_string("opcode")!);

        case NodeKind.CXX_METHOD_DECL:
            n.cxx_method_decl.mangled_name = dstring::new(obj.get_string("mangledName")!);

        case NodeKind.MEMBER_EXPR:
            n.member_expr.referenced_member_decl = parse_id(obj.get_string("referencedMemberDecl")!)!;
            n.member_expr.is_arrow = obj.get_bool("isArrow")!;
            n.member_expr.val_category = parse_value_category(obj.get_string("valueCategory")!)!;

        case NodeKind.CXX_THIS_EXPR:
            n.cxx_this_expr.val_category = parse_value_category(obj.get_string("valueCategory")!)!;

        case NodeKind.IMPLICIT_CAST_EXPR:
            n.implicit_cast_expr.val_category = parse_value_category(obj.get_string("valueCategory")!)!;
            n.implicit_cast_expr.cast_kind = parse_cast_kind(obj.get_string("parseKind")!)!;

        case NodeKind.DECL_REF_EXPR:
            n.decl_ref_expr.val_category = parse_value_category(obj.get_string("valueCategory")!)!;
            n.decl_ref_expr.referenced_decl = parse_id(obj.get("referencedDecl")!.get_string("id")!)!;

        case NodeKind.CXX_RECORD_DECL:
            n.cxx_record_decl.parse(obj)!;

        case NodeKind.TEMPLATE_TYPE_PARM_DECL:
            n.template_type_parm_decl.is_referenced = obj.get_bool("isReferenced")!;
            n.template_type_parm_decl.tag_used = parse_tag(obj.get_string("tagUsed")!)!;
            n.template_type_parm_decl.depth = obj.get_uint("depth")!;
            n.template_type_parm_decl.index = obj.get_uint("index")!;

        default:
    }

    return n;
}

struct Ast 
{
    Node* root;
    Map(<Id, Node*>) id_lookup;
}

def AstIterator = fn void(Node*, void*);
def AstFilter = fn bool(Node*, void*);

fn void Node.iterate(&self, AstIterator iter, void* data)
{
    iter(self, data);
    foreach (&c : self.children)
    {
        c.iterate(iter, data);
    }
}

def NodeCollection = List(<Node*>);
fn void NodeCollection._export_free(&self) @export("c3_clangast_NodeCollection_free")
{
    self.free();
}

fn void Node.filter(&self, AstFilter fil, void* data, NodeCollection* dest)
{
    if (fil(self, data))
    {
        dest.push(self);
    }

    foreach (&c : self.children)
    {
        c.filter(fil, data, dest);
    }
}

fn NodeCollection Ast.find_matching(&self, AstFilter fil, void* data) @export("c3_clangast_find_matching")
{
    NodeCollection coll;
    coll.new_init();
    self.root.filter(fil, data, &coll);
    return coll;
}

fn NodeCollection Node.nodes_in_namespace(&self, String namespace) @export("c3_clangast_nodes_in_namespace")
{
    NodeCollection coll;
    coll.new_init();

    foreach (&c : self.children)
    {
        if (c.kind == NodeKind.NAMESPACE_DECL && c.name.has_value && c.name.value.str_view() == namespace)
        {
            foreach (&cc : c.children)
            {
                coll.push(cc);
            }
        }
    }

    return coll;
}

fn NodeCollection NodeCollection.nodes_with_name(self, String name) @export("c3_clangast_nodes_with_name")
{
    NodeCollection coll;
    coll.new_init();

    foreach (c : self)
    {
        if (c.name.has_value && c.name.value.str_view() == name)
        {
            coll.push(c);
        }
    }

    return coll;
}

fn NodeCollection NodeCollection.template_type_param_decls_sorted(self) @export("c3_clangast_template_type_param_decls_sorted")
{
    NodeCollection coll;
    coll.new_init();

    uint i = 0;
    while (true)
    {
        bool found = false;
        foreach (c : self)
        {
            if (c.kind == NodeKind.TEMPLATE_TYPE_PARM_DECL && c.template_type_parm_decl.index == i)
            {
                coll.push(c);
                found = true;
                break;
            }
        }

        if (!found)
        {
            break;
        }
    }

    return coll;
}

fn void Ast.iterate(&self, AstIterator iter, void* data) @export("c3_clangast_iterate")
{
    self.root.iterate(iter, data);
}

fn void Ast.free(&self) @export("c3_clangast_free")
{
    self.id_lookup.free();
    allocator::free(allocator::heap(), self.root);
}

fn Ast! get_cxx_ast(String file, String[] extra_args) @export("c3_clangast_cxx_ast")
{

    String[] base_args = String[] { "clang++", "-Xclang", "-ast-dump=json" "-fsyntax-only", file };
    String[] args = array::tconcat(base_args, extra_args);

    process::SubProcess! proc = process::create(args);
    (proc ?? AstError.NO_CLANG?)!;
    InStream ast_stream = &&proc.stdout()!;

    if (int result = proc.join()!, result != 0)
    {
        return AstError.CLANG_ERROR?;
    }

    Object* j = json::parse(ast_stream)!;
    defer j.free();

    Node node = parse_node(j)!;

    Ast ast;
    ast.root = allocator::alloc(allocator::heap(), Node);
    *ast.root = node;
    ast.id_lookup = map::new(<Id, Node*>)();

    void* data = &ast.id_lookup;
    ast.iterate(fn void(Node* node, void* data) {
        Map(<Id, Node*>)* map = data;
        map.set(node.id, node);
    }, data);

    return ast;
}
