use std::{
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

use crate::{
    instr::Func,
    mem::{Mem, Obj},
    AssetId, Name, ObjCore, TagId,
};

pub struct Loader {
    asset_list: Vec<*mut Obj>,
    tag_table: HashMap<Name, TagId>,
    prod_table: HashMap<TagId, HashMap<String, usize>>,
    sum_table: HashMap<TagId, HashMap<String, u32>>,
    func_table: HashMap<Name, DispatchList>,
}
type DispatchList = Vec<(Vec<TagExpr>, Arc<Func>)>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TagExpr {
    Has(TagId),
    And(BTreeSet<TagExpr>),
}

impl Default for Loader {
    fn default() -> Self {
        let mut loader = Self {
            asset_list: Vec::new(),
            tag_table: HashMap::new(),
            prod_table: HashMap::new(),
            sum_table: HashMap::new(),
            func_table: HashMap::new(),
        };
        let tag_unit = loader.make_tag("intrinsic.Unit".to_string());
        loader.register_prod(tag_unit, &[]);
        let tag_ref = loader.make_tag("intrinsic.Ref".to_string());
        loader.register_prod(tag_ref, &["content".to_string()]);
        let tag_bool = loader.make_tag("intrinsic.Bool".to_string());
        loader.register_sum(tag_bool, &["True".to_string(), "False".to_string()]);
        loader
    }
}

impl Loader {
    pub fn make_tag(&mut self, name: Name) -> TagId {
        if let Some(id) = self.tag_table.get(&name) {
            *id
        } else {
            let id = self.tag_table.len() as _;
            self.tag_table.insert(name, id);
            id
        }
    }

    pub fn register_func(&mut self, id: Name, param_list: &[TagExpr], func: Func) {
        self.func_table
            .entry(id)
            .or_default()
            .push((param_list.to_vec(), Arc::new(func)));
    }

    pub fn dispatch_call(&self, id: &Name, arg_list: &[&[TagId]]) -> Arc<Func> {
        for (param_list, func) in &self.func_table[id] {
            if param_list.len() != arg_list.len() {
                continue;
            }
            if arg_list
                .iter()
                .zip(param_list.iter())
                .all(|(arg, param)| self.is_compatible(arg, param))
            {
                return func.clone();
            }
        }
        unreachable!()
    }

    fn is_compatible(&self, arg: &[TagId], param: &TagExpr) -> bool {
        match param {
            TagExpr::Has(tag) => arg.iter().any(|arg_tag| arg_tag == tag),
            TagExpr::And(expr_list) => expr_list.iter().all(|expr| self.is_compatible(arg, expr)),
        }
    }

    pub fn register_prod(&mut self, tag: TagId, key_list: &[String]) {
        assert!(self.tag_table.len() > tag as _);
        assert!(!self.sum_table.contains_key(&tag));
        self.prod_table.insert(
            tag,
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.clone(), i))
                .collect(),
        );
    }

    pub fn register_sum(&mut self, tag: TagId, key_list: &[String]) {
        assert!(self.tag_table.len() > tag as _);
        assert!(!self.prod_table.contains_key(&tag));
        self.sum_table.insert(
            tag,
            key_list
                .iter()
                .enumerate()
                .map(|(i, key)| (key.clone(), i as _))
                .collect(),
        );
    }

    pub fn query_tag(&self, name: &str) -> TagId {
        self.tag_table[name]
    }

    pub fn prod_size(&self, tag: TagId) -> usize {
        self.prod_table[&tag].len()
    }

    pub fn prod_offset(&self, tag: TagId, key: &str) -> usize {
        self.prod_table[&tag][key]
    }

    pub fn sum_variant(&self, tag: TagId, key: &str) -> u32 {
        self.sum_table[&tag][key]
    }

    pub fn create_asset<T>(&mut self, asset: T, mem: &Mem) -> AssetId
    where
        T: ObjCore,
    {
        let asset = mem.mutator().new(asset);
        let id = self.asset_list.len();
        self.asset_list.push(asset);
        id as _
    }

    pub fn query_asset(&self, id: AssetId) -> *mut Obj {
        self.asset_list[id as usize]
    }

    pub fn trace(&self, mark: impl FnMut(*mut Obj)) {
        self.asset_list.iter().copied().for_each(mark);
    }
}
