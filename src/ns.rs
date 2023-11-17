use crate::{ast::*, error::RidottoError, scan::Token};
use fnv::FnvHashMap;
use slotmap::{DefaultKey, SlotMap};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VariableLocation {
    Builtin,
    Global,
    Local {
        ns: DefaultKey,
        depth: usize,
        slot: usize,
    },
    Capture {
        from: DefaultKey,
        depth: usize,
        slot: usize,
    },
}

#[derive(Default)]
struct Namespace<'src> {
    parent: Option<DefaultKey>,
    names: FnvHashMap<Token<'src>, VariableLocation>,
    children: Vec<DefaultKey>,
    name: Token<'src>,
    captures: bool,
}

impl std::fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.names.keys()).finish()
    }
}

pub struct NamespaceRepository<'src> {
    namespaces: SlotMap<DefaultKey, Namespace<'src>>,
    global: DefaultKey,
}

impl Default for NamespaceRepository<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'src> NamespaceRepository<'src> {
    pub fn new() -> Self {
        let mut namespaces = SlotMap::default();
        let global = namespaces.insert(Default::default());
        NamespaceRepository { namespaces, global }
    }

    pub fn global_key(&self) -> DefaultKey {
        self.global
    }

    fn global_ns(&self) -> &Namespace<'src> {
        &self.namespaces[self.global_key()]
    }

    fn global_ns_mut<'this>(&'this mut self) -> &'this mut Namespace<'src> {
        let bruh = self.global_key();
        &mut self.namespaces[bruh]
    }

    fn ns<'this>(&'this self, ns: DefaultKey) -> &'this Namespace<'src> {
        &self.namespaces[ns]
    }

    fn ns_mut<'this>(&'this mut self, ns: DefaultKey) -> &'this mut Namespace<'src> {
        &mut self.namespaces[ns]
    }

    // TODO: do we even need this?
    fn depth(&self, mut ns: DefaultKey) -> usize {
        let mut depth = 0;

        while let Some(parent) = self.ns(ns).parent {
            ns = parent;
            depth += 1;
        }

        depth
    }

    pub fn with_parent(&mut self, ns: DefaultKey, name: Token<'src>) -> DefaultKey {
        let key = self.namespaces.insert(Namespace {
            parent: Some(ns),
            name,
            ..Default::default()
        });
        self.namespaces[ns].children.push(key);
        key
    }

    pub fn with_parent_captures(&mut self, ns: DefaultKey, name: Token<'src>) -> DefaultKey {
        let key = self.with_parent(ns, name);
        self.ns_mut(key).captures = true;
        key
    }

    pub fn find(&mut self, ns: DefaultKey, name: Token<'src>) -> Option<VariableLocation> {
        tracing::debug!("find {}", name.lexeme);
        self.get_rec(ns, name, true).map(|(_, loc)| loc)
    }

    pub fn get(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Option<(Token<'src>, VariableLocation)> {
        tracing::debug!("find {}", name.lexeme);
        self.get_rec(ns, name, true)
    }

    fn get_rec(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
        top: bool,
    ) -> Option<(Token<'src>, VariableLocation)> {
        tracing::trace!(
            "{} {} {:?}",
            self.ns(ns).name.lexeme,
            self.ns(ns).name.pos,
            self.ns(ns)
                .names
                .keys()
                .map(|token| token.lexeme)
                .collect::<Vec<_>>()
        );

        // check the current namespace
        if let Some((token, loc)) = self.ns(ns).names.get_key_value(&name) {
            tracing::trace!("I have {} at {}", token.lexeme, token.pos);
            return Some((*token, *loc));
        }

        // check the parent namespace if it exists
        if let Some(parent) = self.ns(ns).parent {
            tracing::trace!("checking parent for {}", name.lexeme);
            let def = self.get_rec(parent, name, false);

            // if the parent contains the name and it's a local variable, add it is a capture to
            // the current namespace
            if let Some((
                def,
                VariableLocation::Local {
                    ns: from,
                    depth,
                    slot,
                },
            )) = def
            {
                if top && self.ns(ns).captures {
                    tracing::debug!("capturing {}", def.lexeme);
                    self.ns_mut(ns)
                        .names
                        .insert(def, VariableLocation::Capture { from, depth, slot });
                }
            }

            return def;
        }

        tracing::trace!("checking global ns for {}", name.lexeme);
        return self
            .global_ns()
            .names
            .get_key_value(&name)
            .map(|(token, loc)| (*token, *loc));
    }

    pub fn new_builtin(&mut self, name: Token<'src>) {
        tracing::trace!("new builtin {}", name.lexeme);

        if let Some(_) = self.global_ns().names.get(&name) {
            return;
        }

        self.global_ns_mut()
            .names
            .insert(name, VariableLocation::Builtin);
    }

    pub fn new_global(&mut self, name: Token<'src>) -> VariableLocation {
        tracing::trace!("new global {}", name.lexeme);

        if let Some(location) = self.global_ns().names.get(&name) {
            return *location;
        }

        let location = VariableLocation::Global;
        self.global_ns_mut().names.insert(name, location);
        location
    }

    fn new_local(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Result<VariableLocation, RidottoError> {
        tracing::trace!("new local {}", name.lexeme);

        if !self.ns(ns).names.contains_key(&name) {
            // TODO
            let location = VariableLocation::Local {
                ns,
                depth: self.depth(ns),
                slot: self.ns(ns).names.len(),
            };
            tracing::trace!("insert {} {:?}", name.lexeme, location);
            self.ns_mut(ns).names.insert(name, location);
            return Ok(location);
        }

        todo!("local already defined: '{}'", name.lexeme);
        //Err(make_error!(SyntaxError)
        //    .msg_string(format!("local already defined: '{}'", name.lexeme))
        //    .pos(name.pos))
    }

    pub fn new_variable(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Result<VariableLocation, RidottoError> {
        tracing::debug!("new var {}", name.lexeme);
        if ns == self.global_key() {
            Ok(self.new_global(name))
        } else {
            self.new_local(ns, name)
        }
    }

    pub fn debug(&self) {
        self.hmm_rec(0, self.global_key())
    }

    fn hmm_rec(&self, indent: usize, ns: DefaultKey) {
        print!(
            "{:<35}",
            format!("{} {}", self.ns(ns).name.lexeme, self.ns(ns).name.pos)
        );
        print!("{:>indent$}[", "");
        for (i, (name, loc)) in self.ns(ns).names.iter().enumerate() {
            print!("{}", name.lexeme);
            if let VariableLocation::Capture { from, .. } = loc {
                print!(" (from {} in {})", name.pos, self.ns(*from).name.lexeme);
            }
            if i + 1 != self.ns(ns).names.len() {
                print!(", ");
            }
        }
        println!("]");

        for child in self.ns(ns).children.iter() {
            self.hmm_rec(indent + 2, *child);
        }
    }
}

impl std::fmt::Debug for NamespaceRepository<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamespaceRepository")
            .field("globals", self.global_ns())
            .field("children", &self.namespaces.values().collect::<Vec<_>>())
            .finish()
    }
}

fn err(variable: Token) -> Result<(), RidottoError> {
    return Err(RidottoError::unknown_variable(variable));
}

pub fn analyze_ns<'src>(
    ast: &'src [Item<'src>],
) -> Result<NamespaceRepository<'src>, RidottoError> {
    let mut repo = NamespaceRepository::new();

    for item in ast {
        let global = repo.global_key();
        analyze_ns_item(&mut repo, global, item)?;
    }

    Ok(repo)
}

fn analyze_ns_item<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    item: &'src Item<'src>,
) -> Result<(), RidottoError> {
    match item {
        Item::ItemFunction(func) => {
            repo.new_variable(ns, func.head.name.lowercase)?;
            let inner_ns = repo.with_parent(ns, func.head.name.lowercase);
            for arg in func.head.args.iter() {
                repo.new_variable(inner_ns, arg.name.lowercase)?;
            }
            for stmt in func.body.iter() {
                analyze_ns_stmt(repo, inner_ns, stmt)?;
            }
        }

        _ => {},
    }

    Ok(())
}

fn analyze_ns_stmt<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    stmt: &'src Stmt<'src>,
) -> Result<(), RidottoError> {
    match stmt {
        Stmt::Binding { pattern, value } => {
            analyze_ns_pattern(repo, ns, pattern)?;
            analyze_ns_expr(repo, ns, value)?;
        }

        Stmt::Expr(expr) => analyze_ns_expr(repo, ns, expr)?,
    }

    Ok(())
}

fn analyze_ns_pattern<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    pattern: &'src Pattern<'src>,
) -> Result<(), RidottoError> {
    match pattern {
        Pattern::Any => {}

        Pattern::Binding { name } => {
            repo.new_variable(ns, name.lowercase)?;
        }

        Pattern::MutableBinding { name } => {
            repo.new_variable(ns, name.lowercase)?;
        }

        Pattern::Alternate { left, right } => {
            analyze_ns_pattern(repo, ns, left)?;
            analyze_ns_pattern(repo, ns, right)?;
        }

        Pattern::StructDestructure { bindings, .. } => {
            for binding in bindings.iter() {
                match binding {
                    StructFieldPattern::Named { name, value } => {
                        analyze_ns_pattern(repo, ns, value)?;
                    }
                        //repo.new_variable(ns, name.lowercase)?;

                    StructFieldPattern::Shorthand { name } => {
                        repo.new_variable(ns, name.lowercase)?;
                    }

                    StructFieldPattern::MutableShorthand { name } => {
                        repo.new_variable(ns, name.lowercase)?;
                    }

                    StructFieldPattern::Rest => {}
                }
            }
        }

        Pattern::TupleDestructure { bindings, .. } | Pattern::Tuple { bindings } => {
            for binding in bindings.iter() {
                analyze_ns_pattern(repo, ns, binding)?;
            }
        }

        Pattern::EnumVariant { .. } => {}
    }

    Ok(())
}

fn analyze_ns_expr<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    expr: &'src Expr<'src>,
) -> Result<(), RidottoError> {
    match expr {
        Expr::Block { stmts } => {
            let block = repo.with_parent(ns, expr.token());
            for stmt in stmts.iter() {
                analyze_ns_stmt(repo, block, stmt)?;
            }
        }

        Expr::Literal { .. } => {}

        Expr::Paren { expr } => {
            analyze_ns_expr(repo, ns, expr)?;
        }

        Expr::GetFrom { object, .. } => {
            analyze_ns_expr(repo, ns, object)?;
        }

        Expr::TupleIndex { object, .. } => {
            analyze_ns_expr(repo, ns, object)?;
        }

        Expr::Call { callee, args } => {
            analyze_ns_expr(repo, ns, callee)?;
            for arg in args.iter() {
                analyze_ns_expr(repo, ns, arg)?;
            }
        }

        // TODO check type namespace
        Expr::StructInstantiate { values, .. } => {
            for value in values.iter() {
                match value {
                    StructField::Named { value, .. } => analyze_ns_expr(repo, ns, value)?,
                    StructField::Shorthand { name } => {
                        if repo.find(ns, name.lowercase).is_none() {
                            return err(name.lowercase);
                        }
                    }
                    StructField::Spread { value } => analyze_ns_expr(repo, ns, value)?,
                }
            }
        }

        // TODO check type namespace
        Expr::TupleInstantiate { values, .. } => {
            for value in values.iter() {
                analyze_ns_expr(repo, ns, value)?;
            }
        }

        Expr::Tuple { values } => {
            for value in values.iter() {
                analyze_ns_expr(repo, ns, value)?;
            }
        }

        Expr::Unary { rhs, .. } => {
            analyze_ns_expr(repo, ns, rhs)?;
        }

        Expr::Binary { lhs, rhs, .. } => {
            analyze_ns_expr(repo, ns, lhs)?;
            analyze_ns_expr(repo, ns, rhs)?;
        }

        Expr::Variable { variable } => {
            if repo.find(ns, variable.lowercase).is_none() {
                return err(variable.lowercase);
            }
        }

        // TODO check type namespace
        Expr::TypeName { .. } => {}

        Expr::If {
            cond,
            block,
            else_block,
        } => {
            analyze_ns_expr(repo, ns, cond)?;
            let block_ns = repo.with_parent(ns, cond.token());

            for stmt in block.iter() {
                analyze_ns_stmt(repo, block_ns, stmt)?;
            }

            if let Some(else_block) = else_block {
                let else_block_ns = repo.with_parent(ns, cond.token());
                for stmt in else_block.iter() {
                    analyze_ns_stmt(repo, else_block_ns, stmt)?;
                }
            }
        }

        Expr::For {} => {}

        Expr::Match {
            discriminant,
            branches,
        } => {
            analyze_ns_expr(repo, ns, &discriminant)?;

            for branch in branches.iter() {
                let branch_ns = repo.with_parent(ns, expr.token());
                analyze_ns_pattern(repo, branch_ns, &branch.pattern)?;
                if let Some(guard) = branch.guard.as_ref() {
                    analyze_ns_expr(repo, branch_ns, guard)?;
                }
                analyze_ns_expr(repo, branch_ns, &branch.body)?;
            }
        }
    }

    Ok(())
}
