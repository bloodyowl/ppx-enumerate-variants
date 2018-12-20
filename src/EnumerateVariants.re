open Asttypes;
open Parsetree;
open Ast_mapper;

let rec structureMapper = (mapper, structure: structure) =>
  switch (structure) {
  | [] => structure
  | [
      {
        pstr_desc:
          Pstr_type([
            {
              ptype_attributes,
              ptype_kind: Ptype_abstract,
              ptype_manifest:
                Some({ptyp_desc: Ptyp_variant(variants, _, _)}),
            } as typeDeclaration,
            ..._,
          ]),
      } as desc,
      ...tl,
    ]
      when
        variants->Belt.List.every(item =>
          switch (item) {
          | Rtag(_, _, true, _) => true
          | _ => false
          }
        )
        && ptype_attributes
           ->Belt.List.getBy((({txt}, _)) => txt == "enumerate_variants")
           ->Belt.Option.isSome =>
    let allVariants =
      variants->Belt.List.keepMap(item =>
        switch (item) {
        | Rtag(label, _, true, _) => Some(label)
        | _ => None
        }
      );
    let {txt} = typeDeclaration.ptype_name;
    let cleanAttributes =
      ptype_attributes->Belt.List.keep((({txt}, _)) =>
        txt != "enumerate_variants"
      );
    [
      {
        ...desc,
        pstr_desc:
          Pstr_type([
            {...typeDeclaration, ptype_attributes: cleanAttributes},
          ]),
      },
      Ast_helper.Str.mk(
        Pstr_value(
          Nonrecursive,
          [
            {
              pvb_pat:
                Ast_helper.Pat.mk(
                  Ppat_constraint(
                    Ast_helper.Pat.mk(
                      Ppat_var({
                        loc: Ast_helper.default_loc^,
                        txt: txt ++ "All",
                      }),
                    ),
                    Ast_helper.Typ.mk(
                      Ptyp_constr(
                        {txt: Lident("array"), loc: Ast_helper.default_loc^},
                        [
                          Ast_helper.Typ.mk(
                            Ptyp_constr(
                              {
                                txt: Lident(txt),
                                loc: Ast_helper.default_loc^,
                              },
                              [],
                            ),
                          ),
                        ],
                      ),
                    ),
                  ),
                ),
              pvb_expr:
                Ast_helper.Exp.array(
                  allVariants->Belt.List.map(string =>
                    Ast_helper.Exp.mk(Pexp_variant(string, None))
                  ),
                ),
              pvb_attributes: [],
              pvb_loc: Ast_helper.default_loc^,
            },
          ],
        ),
      ),
      ...structureMapper(mapper, tl),
    ];
  | [other, ...tl] => [other, ...structureMapper(mapper, tl)]
  };

let enumerate_variants_mapper = {
  ...default_mapper,
  structure: (mapper, structure) =>
    default_mapper.structure(mapper, structureMapper(mapper, structure)),
};

let () = register("ppx_enumerate_variants", _ => enumerate_variants_mapper);
