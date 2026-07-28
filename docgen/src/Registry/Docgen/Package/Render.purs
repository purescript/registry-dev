module Registry.Docgen.Package.Render where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold, foldMap, intercalate)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Registry.Docgen.Commonmark (renderMarkdownHTML)
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocPackage(..), DocReexport(..), InfixAlias(..), ModuleName, OperatorName(..), Qualified(..), RawRange(..), Readme(..), SourcePos(..), SourceSpan(..), TypeName(..), ValueName(..), isPrim)
import Registry.Docgen.HTML (HTML)
import Registry.Docgen.HTML as H
import Registry.Docgen.Package.Render.Code (CodeRenderer, renderChildDeclConstructor, renderChildDeclInstance, renderChildDeclTypeClassMember, renderDeclData, renderDeclForeignData, renderDeclInfix, renderDeclType, renderDeclTypeClass, renderDeclValue)
import Registry.Docgen.Package.Types (ModuleRef(..), Namespace(..))
import Registry.License as License
import Registry.Location (Location)
import Registry.Location as Location
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version as Version

type Link =
  { href :: String
  , title :: String
  }

type PackageLinker =
  { getModuleLink :: ModuleName -> Link
  , getPackageLink :: PackageName -> Link
  , getRefLink :: { moduleName :: ModuleName, namespace :: Namespace, ref :: String } -> Link
  , getSourceLink :: { moduleName :: ModuleName, sourceSpan :: SourceSpan } -> Link
  }

type RenderedDeclInfo =
  { anchorId :: String
  , content :: HTML
  , title :: String
  }

type RenderedChildDecl =
  { anchorId :: String
  , comments :: Maybe String
  , content :: HTML
  , title :: String
  }

renderModuleName :: ModuleName -> HTML
renderModuleName =
  unwrap
    >>> String.split (Pattern ".")
    >>> map H.text
    >>> intercalate (H.text "." <> H.wbr)

namespaceAnchor :: Namespace -> String
namespaceAnchor = case _ of
  NSType -> "t:"
  NSValue -> "v:"

defaultPackageLinker :: DocPackage -> PackageLinker
defaultPackageLinker
  ( DocPackage
      { modules
      , name: packageName
      , resolvedDependencies
      , resolvedModulePackages
      , version
      }
  ) =
  { getModuleLink, getPackageLink, getRefLink, getSourceLink }
  where
  versionMap =
    Map.insert packageName version resolvedDependencies

  moduleMap =
    Map.union resolvedModulePackages
      $ Map.fromFoldable
      $ map (\(DocModule { name }) -> Tuple name packageName) modules

  getModuleLink moduleName
    | isPrim moduleName =
        { href: intercalate "/"
            [ ""
            , "builtins"
            , "docs"
            , unwrap moduleName
            ]
        , title: unwrap moduleName
        }
    | otherwise = fold do
        resolvedPackageName <- Map.lookup moduleName moduleMap
        let { href, title } = getPackageLink resolvedPackageName
        let printedModuleName = unwrap moduleName
        pure
          { href: intercalate "/"
              [ href
              , "docs"
              , printedModuleName
              ]
          , title: title <> " " <> printedModuleName
          }

  getPackageLink name = fold do
    resolvedVersion <- Map.lookup name versionMap
    let printedName = PackageName.print name
    let printedVersion = Version.print resolvedVersion
    pure
      { href: intercalate "/"
          [ ""
          , "packages"
          , printedName
          , printedVersion
          ]
      , title: printedName <> "@" <> printedVersion
      }

  getRefLink { moduleName, namespace, ref } = do
    let { href, title } = getModuleLink moduleName
    { href: href <> "#" <> namespaceAnchor namespace <> ref
    , title: title <> "." <> ref
    }

  getSourceLink
    { moduleName
    , sourceSpan: SourceSpan
        { path
        , start: SourcePos { line: startLine }
        , end: SourcePos { line: endLine }
        }
    } = fold do
    resolvedPackageName <- Map.lookup moduleName moduleMap
    resolvedVersion <- Map.lookup resolvedPackageName versionMap
    let printedPackageName = PackageName.print resolvedPackageName
    let printedVersion = Version.print resolvedVersion
    pure
      { href: intercalate "/"
          [ "https://www.purescript.org/registry-package-viewer/#"
          , printedPackageName
          , printedVersion
          , path <> "#" <> show startLine <> "-" <> show endLine
          ]
      , title: printedPackageName <> "@" <> printedVersion <> "/" <> path
      }

htmlCodeRenderer :: PackageLinker -> ModuleName -> CodeRenderer HTML
htmlCodeRenderer { getRefLink } currentModule =
  { keyword: \str ->
      H.span
        [ H.class_ "keyword" ]
        [ H.text str ]
  , label: \str -> do
      let unquoted = SCU.drop 1 (SCU.dropRight 1 str)
      H.span
        [ H.class_ "row-label" ]
        [ H.text if isIdent unquoted then unquoted else str ]
  , line: \indent children ->
      H.div
        [ H.class_ $ "indent-" <> show indent ]
        [ children ]
  , reference: \mod ref -> do
      let
        label =
          H.span
            [ H.class_ if isIdent ref then "ident" else "ctor" ]
            [ H.text ref ]
      case mod of
        Local ->
          label
        ModuleRef moduleName namespace -> do
          let
            { href, title } = getRefLink
              { moduleName: fromMaybe currentModule moduleName
              , namespace
              , ref
              }
          H.a
            [ H.href href, H.title title ]
            [ label ]
  , role: \str ->
      H.span
        [ H.class_ "role" ]
        [ H.text str ]
  , space:
      H.text " "
  , syntax: \str ->
      H.span
        [ H.class_ "syntax" ]
        [ H.text str ]
  }
  where
  isIdent =
    Regex.test $ unsafeRegex "^[a-z_][a-zA-Z0-9_']*" noFlags

renderDocument :: { body :: HTML, title :: String } -> HTML
renderDocument { body, title } =
  H.doctype <>
    H.html [ H.lang "en" ]
      [ H.head
          [ H.meta [ H.attr "charset" "utf-8" ]
          , H.htmlTitle
              [ H.text title ]
          , H.link
              [ H.href "https://fonts.googleapis.com/css?family=Roboto+Mono|Roboto:300,400,400i,700,700i"
              , H.type_ "text/css"
              , H.rel "stylesheet"
              ]
          , H.link
              [ H.href "https://pursuit.purescript.org/static/res/css/normalize.css?etag=fKzu1nci"
              , H.type_ "text/css"
              , H.rel "stylesheet"
              ]
          , H.link
              [ H.href "https://pursuit.purescript.org/static/res/css/pursuit.css?etag=5eIKlitR"
              , H.type_ "text/css"
              , H.rel "stylesheet"
              ]
          , H.link
              [ H.href "https://pursuit.purescript.org/static/res/css/extra.css?etag=d4aey1o-"
              , H.type_ "text/css"
              , H.rel "stylesheet"
              ]
          ]
      , H.body [] [ body ]
      ]

renderContainer :: { anchorId :: String, content :: HTML } -> HTML
renderContainer { anchorId, content } =
  H.main
    [ H.class_ "container clearfix"
    , H.id anchorId
    ]
    [ content ]

renderPackageIndex :: PackageLinker -> DocPackage -> HTML
renderPackageIndex linker (DocPackage { dependencies, license, location, modules, name, readme }) =
  pageLayout
    { label: H.text "Package"
    , title: H.text $ PackageName.print name
    , main: fold
        [ groupedList
            [ Tuple (H.text "Repository")
                [ renderRepository location
                ]
            , Tuple (H.text "License")
                [ H.text $ License.print license ]
            ]
        , H.forEach readme \(Readme { content, extension }) -> do
            case foldMap String.toLower extension of
              "md" ->
                H.article
                  [ H.class_ "readme readme--markdown" ]
                  [ renderMarkdownHTML { safe: true } content ]
              _ ->
                H.article
                  [ H.class_ "readme readme--plain" ]
                  [ H.text content ]
        ]
    , sidebar: fold
        [ renderModuleList linker modules
        , renderDependencyList linker dependencies
        ]
    }

renderModule :: PackageLinker -> DocPackage -> DocModule -> HTML
renderModule
  linker
  (DocPackage { location, modules, name: packageName })
  ( DocModule
      { comments
      , declarations
      , name: moduleName
      , reexports
      }
  ) =
  pageLayout
    { label: H.text "module"
    , title: H.text $ unwrap moduleName
    , main: fold
        [ groupedList
            [ Tuple (H.text "Package")
                [ H.text $ PackageName.print packageName ]
            , Tuple (H.text "Repository")
                [ renderRepository location ]
            ]
        , H.forEach comments $ renderMarkdownHTML { safe: true }
        , H.forEach declarations $ renderDeclaration linker code moduleName
        , H.forEach reexports $ renderReexport linker code
        ]
    , sidebar: renderModuleList linker modules
    }
  where
  code :: CodeRenderer HTML
  code = htmlCodeRenderer linker moduleName

renderDeclaration :: PackageLinker -> CodeRenderer HTML -> ModuleName -> DocDeclaration -> HTML
renderDeclaration { getSourceLink } code moduleName (DocDeclaration { children, info, comments, sourceSpan }) = do
  let { anchorId, content, title } = renderDeclarationInfo code info
  H.div
    [ H.class_ "decl"
    , H.id anchorId
    ]
    [ H.h3
        [ H.class_ "decl__title clearfix" ]
        [ H.a
            [ H.class_ "decl__anchor"
            , H.href $ "#" <> anchorId
            ]
            [ H.text "#" ]
        , H.span []
            [ H.text title ]
        , H.forEach sourceSpan \span -> do
            let { href, title } = getSourceLink { moduleName, sourceSpan: span }
            H.a
              [ H.class_ "decl__source"
              , H.href href
              , H.title title
              ]
              [ H.text "Source" ]
        ]
    , H.div
        [ H.class_ "decl__body" ]
        [ H.pre
            [ H.class_ "decl__signature" ]
            [ H.code []
                [ content ]
            ]
        , H.forEach comments $ renderMarkdownHTML { safe: true }
        , H.forEach (renderChildConstructors code children) \items -> fold
            [ H.h4 []
                [ H.text "Constructors" ]
            , H.ul []
                [ H.forEach items renderChildDeclaration ]
            ]
        , H.forEach (renderChildMembers code children) \items -> fold
            [ H.h4 []
                [ H.text "Members" ]
            , H.ul []
                [ H.forEach items renderChildDeclaration ]
            ]
        , H.forEach (renderChildInstances code children) \items -> fold
            [ H.h4 []
                [ H.text "Instances" ]
            , H.ul []
                [ H.forEach items renderChildDeclaration ]
            ]
        ]
    ]

renderReexport :: PackageLinker -> CodeRenderer HTML -> DocReexport -> HTML
renderReexport linker@{ getModuleLink } code (DocReexport { declarations, moduleName }) = do
  let { href, title } = getModuleLink moduleName
  fold
    [ H.h2
        [ H.class_ "reexports" ]
        [ H.text "Re-exports from "
        , H.a
            [ H.href href
            , H.title title
            ]
            [ H.text $ unwrap moduleName ]
        ]
    , H.forEach declarations $ renderDeclaration linker code moduleName
    ]

renderDeclarationInfo :: CodeRenderer HTML -> DocDeclarationInfo -> RenderedDeclInfo
renderDeclarationInfo code = case _ of
  DeclValue rep -> do
    let (Qualified { name: ValueName title }) = rep.name
    { anchorId: namespaceAnchor NSValue <> title
    , content: renderDeclValue code rep
    , title
    }
  DeclData rep -> do
    let (Qualified { name: TypeName title }) = rep.name
    { anchorId: namespaceAnchor NSType <> title
    , content: renderDeclData code rep
    , title
    }
  DeclType rep -> do
    let (Qualified { name: TypeName title }) = rep.name
    { anchorId: namespaceAnchor NSType <> title
    , content: renderDeclType code rep
    , title
    }
  DeclTypeClass rep -> do
    let (Qualified { name: TypeName title }) = rep.name
    { anchorId: namespaceAnchor NSType <> title
    , content: renderDeclTypeClass code rep
    , title
    }
  DeclInfix rep -> do
    let
      (Qualified { name: OperatorName title }) = rep.name
      namespace = case rep.alias of
        AliasType _ -> NSType
        _ -> NSValue
    { anchorId: namespaceAnchor namespace <> title
    , content: renderDeclInfix code rep
    , title
    }
  DeclForeignData rep -> do
    let (Qualified { name: TypeName title }) = rep.name
    { anchorId: namespaceAnchor NSType <> title
    , content: renderDeclForeignData code rep
    , title
    }

renderChildDeclaration :: RenderedChildDecl -> HTML
renderChildDeclaration childProps =
  H.li
    [ H.id childProps.anchorId ]
    [ childProps.content
    , H.forEach childProps.comments \md ->
        H.div
          [ H.class_ "decl__child_comments" ]
          [ renderMarkdownHTML { safe: true } md ]
    ]

renderChildConstructors :: CodeRenderer HTML -> Array DocChildDeclaration -> Maybe (NonEmptyArray RenderedChildDecl)
renderChildConstructors code = NonEmptyArray.fromArray <<< Array.mapMaybe case _ of
  DocChildDeclaration { comments, info: ChildDeclConstructor rep } -> do
    let (Qualified { name: DataConstructorName title }) = rep.name
    let anchorId = namespaceAnchor NSValue <> title
    Just
      { anchorId
      , comments
      , content: H.code [] [ renderChildDeclConstructor code rep ]
      , title
      }
  _ ->
    Nothing

renderChildInstances :: CodeRenderer HTML -> Array DocChildDeclaration -> Maybe (NonEmptyArray RenderedChildDecl)
renderChildInstances code = NonEmptyArray.fromArray <<< Array.mapMaybe case _ of
  DocChildDeclaration { comments, info: ChildDeclInstance rep } -> do
    let (Qualified { name: ValueName title }) = rep.name
    let anchorId = namespaceAnchor NSValue <> title
    Just
      { anchorId
      , comments
      , content: H.code [] [ renderChildDeclInstance code rep ]
      , title
      }
  _ ->
    Nothing

renderChildMembers :: CodeRenderer HTML -> Array DocChildDeclaration -> Maybe (NonEmptyArray RenderedChildDecl)
renderChildMembers code = NonEmptyArray.fromArray <<< Array.mapMaybe case _ of
  DocChildDeclaration { comments, info: ChildDeclTypeClassMember rep } -> do
    let (Qualified { name: ValueName title }) = rep.name
    let anchorId = namespaceAnchor NSValue <> title
    Just
      { anchorId
      , comments
      , content: H.code [] [ renderChildDeclTypeClassMember code rep ]
      , title
      }
  _ ->
    Nothing

renderRepository :: Location -> HTML
renderRepository = case _ of
  Location.GitHub { owner, repo } ->
    H.a
      [ H.href $ "https://github.com/" <> owner <> "/" <> repo ]
      [ H.text $ "github.com/" <> owner <> "/" <> repo ]
  Location.Git { url } ->
    H.a
      [ H.href url ]
      [ H.text url ]

renderModuleList :: PackageLinker -> Array DocModule -> HTML
renderModuleList { getModuleLink } modules =
  groupedList
    [ Tuple (H.text "Modules") $
        renderModuleLink <$> modules
    ]
  where
  renderModuleLink (DocModule { name }) = do
    let { href, title } = getModuleLink name
    H.a
      [ H.href href
      , H.title title
      ]
      [ renderModuleName name ]

renderDependencyList :: PackageLinker -> Map PackageName RawRange -> HTML
renderDependencyList { getPackageLink } dependencies =
  groupedList
    [ Tuple (H.text "Dependencies") $
        renderDependencyLink <$> Map.toUnfoldable dependencies
    ]
  where
  renderDependencyLink (Tuple packageName (RawRange range)) = do
    let { href, title } = getPackageLink packageName
    H.div
      [ H.class_ "deplink" ]
      [ H.a
          [ H.class_ "deplink__link"
          , H.href href
          , H.title title
          ]
          [ H.text $ PackageName.print packageName ]
      , H.span
          [ H.class_ "deplink__version" ]
          [ H.text range ]
      ]

pageLayout :: { label :: HTML, main :: HTML, sidebar :: HTML, title :: HTML } -> HTML
pageLayout { label, main, sidebar, title } = fold
  [ H.div
      [ H.class_ "page-title clearfix" ]
      [ H.div
          [ H.class_ "page-title__label" ]
          [ label ]
      , H.h1
          [ H.class_ "page-title__title" ]
          [ title ]
      ]
  , H.div
      [ H.class_ "col col--main" ]
      [ main ]
  , H.div
      [ H.class_ "col col--aside" ]
      [ sidebar ]
  ]

groupedList :: Array (Tuple HTML (Array HTML)) -> HTML
groupedList items =
  H.dl
    [ H.class_ "grouped-list" ]
    (renderItem <$> items)
  where
  renderItem (Tuple title children) = fold
    [ H.dt
        [ H.class_ "grouped-list__title" ]
        [ title ]
    , H.forEach children \item ->
        H.dd
          [ H.class_ "grouped-list__item" ]
          [ item ]
    ]
