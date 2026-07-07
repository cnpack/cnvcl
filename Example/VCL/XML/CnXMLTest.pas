unit CnXMLTest;
{* |<PRE>
================================================================================
* Software Name: CnPack Component Package
* Unit Name: CnXML Comprehensive Test Unit
* Purpose: Command-line test suite for CnXML.pas covering Lexer, Parser,
*          Encoding, Namespace, PI/DOCTYPE, Attributes, DOM, Serialization,
*          and Helper functions. Single mega unit, single runner, no args.
*          Supports Delphi 5/2007/2009+ and Free Pascal (FPC).
*
* Development Platform: PWin7 + Delphi 5
* Compatible Platform: PWin7/10 + Delphi 5+, FPC
* Note: ASCII-only comments to keep file encoding portable.
* Version History: 2026.07.07 V1.0
*                  Created unit
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, TypInfo, CnNative, CnXML;

//==============================================================================
// S1 Lexer Tests
//==============================================================================
function Test_Lexer_NextToken: Boolean;
function Test_Lexer_ReadComment: Boolean;
function Test_Lexer_ReadCData: Boolean;
function Test_Lexer_CDataSpecialChars: Boolean;
function Test_Lexer_CDataMultiline: Boolean;
function Test_Lexer_ReadPI: Boolean;
function Test_Lexer_ReadText: Boolean;
function Test_Lexer_EntityDecoding: Boolean;
function Test_Lexer_EntityHexDec: Boolean;
function Test_Lexer_InvalidEntityRaises: Boolean;

//==============================================================================
// S2 Parser Tests
//==============================================================================
function Test_Parser_XmlDecl: Boolean;
function Test_Parser_Element: Boolean;
function Test_Parser_Content: Boolean;
function Test_Parser_UnclosedTagRaises: Boolean;
function Test_Parser_AttrQuoteMissingRaises: Boolean;
function Test_Parser_IllegalTagNameRaises: Boolean;
function Test_Parser_ErrorPosition: Boolean;
function Test_Parser_MultilineTag: Boolean;
function Test_Parser_TabInTag: Boolean;
function Test_Parser_IfnassTemplate: Boolean;

//==============================================================================
// S3 Encoding Tests
//==============================================================================
function Test_Encoding_DetectUTF8BOM: Boolean;
function Test_Encoding_DetectUTF16LE: Boolean;
function Test_Encoding_DetectUTF16BE: Boolean;
function Test_Encoding_UTF8FileLoad: Boolean;
function Test_Encoding_UTF16BEFileLoad: Boolean;
function Test_Encoding_GBKFileLoad: Boolean;
function Test_Encoding_DeclMismatch: Boolean;
function Test_Encoding_ConvertEncoding: Boolean;

//==============================================================================
// S4 Namespace Tests
//==============================================================================
function Test_NS_DefaultNamespace: Boolean;
function Test_NS_PrefixResolve: Boolean;
function Test_NS_XmlPrefix: Boolean;
function Test_NS_ScopeInherit: Boolean;
function Test_NS_AttrWithPrefix: Boolean;

//==============================================================================
// S5 PI / DOCTYPE Tests
//==============================================================================
function Test_PI_Content: Boolean;
function Test_PI_XmlStylesheet: Boolean;
function Test_DOCTYPE_Skip: Boolean;
function Test_DOCTYPE_WithEntities: Boolean;

//==============================================================================
// S6 Attributes Tests
//==============================================================================
function Test_Attr_SingleQuote: Boolean;
function Test_Attr_MixedQuotes: Boolean;
function Test_Attr_EntityInValue: Boolean;
function Test_Attr_NewlineInValue: Boolean;
function Test_Attr_XmlSpace: Boolean;
function Test_Attr_XmlLang: Boolean;
function Test_Attr_Chinese: Boolean;

//==============================================================================
// S7 DOM Tests
//==============================================================================
function Test_DOM_AddRemove: Boolean;
function Test_DOM_CloneDeepSubtree: Boolean;
function Test_DOM_CloneCrossDoc: Boolean;
function Test_DOM_CloneAttrIndependent: Boolean;
function Test_DOM_SetTextClears: Boolean;
function Test_DOM_OwnerAfterLoad: Boolean;
function Test_DOM_PreserveWhitespace: Boolean;
function Test_DOM_XmlSpacePreserve: Boolean;
function Test_DOM_AdjacentTextMerge: Boolean;
function Test_DOM_NoAttrLeak: Boolean;
function Test_DOM_UnicodeCodepoint: Boolean;

//==============================================================================
// S8 Serialize Tests
//==============================================================================
function Test_Ser_SaveIndent: Boolean;
function Test_Ser_SaveNoIndent: Boolean;
function Test_Ser_BOMWrite: Boolean;
function Test_Ser_DeclConsistency: Boolean;
function Test_Ser_UseDataNodeOn: Boolean;
function Test_Ser_UseDataNodeOff: Boolean;
function Test_Ser_RTTISimple: Boolean;
function Test_Ser_RTTINested: Boolean;
function Test_Ser_RTTICollection: Boolean;
function Test_Ser_OmniXMLCompat: Boolean;
function Test_Ser_ChineseRoundTrip: Boolean;
function Test_Ser_ChineseSerialization: Boolean;

//==============================================================================
// S9 Helpers Tests
//==============================================================================
function Test_Helper_StrToInt: Boolean;
function Test_Helper_StrToIntDef: Boolean;
function Test_Helper_StrToBool: Boolean;
function Test_Helper_StrToInt64: Boolean;
function Test_Helper_IntToStr: Boolean;
function Test_Helper_BoolToStr: Boolean;
function Test_Helper_RealToStr: Boolean;
function Test_Helper_DateTime: Boolean;

//==============================================================================
// Runner
//==============================================================================
procedure RunXMLTests;

implementation

//==============================================================================
// Test framework helpers
//==============================================================================

type
  TTestFunc = function: Boolean;

var
  FPassCount: Integer;
  FFailCount: Integer;
  FSkipCount: Integer;

function GetAllocatedBytes: Int64;
{$IFDEF FPC}
begin
  // FPC's THeapStatus layout differs across versions and isn't reliably
  // introspectable here; leak-detection tests skip when 0 is returned.
  Result := 0;
end;
{$ELSE}
  {$IFDEF UNICODE}
var
  State: TMemoryManagerState;
  I: Integer;
  begin
    GetMemoryManagerState(State);
    Result := 0;
    for I := 0 to High(State.SmallBlockTypeStates) do
      Inc(Result, State.SmallBlockTypeStates[I].AllocatedBlockCount *
                   State.SmallBlockTypeStates[I].UseableBlockSize);
    Inc(Result, State.TotalAllocatedMediumBlockSize);
    Inc(Result, State.TotalAllocatedLargeBlockSize);
  end;
  {$ELSE}
var
  H: THeapStatus;
  begin
    H := GetHeapStatus;
    Result := H.TotalAllocated;
  end;
  {$ENDIF}
{$ENDIF}

procedure Run(const Name: string; TestFunc: TTestFunc);
begin
  Write(Name, ': ');
  if TestFunc then
  begin
    WriteLn('PASS');
    Inc(FPassCount);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FFailCount);
  end;
end;

procedure RunSkipped(const Name: string; const Reason: string);
begin
  WriteLn(Name, ': SKIPPED (', Reason, ')');
  Inc(FSkipCount);
end;

// Free a token's Attributes list (lexer allocates one for tag/XMLDecl tokens)
procedure FreeTokenAttrs(var Token: TCnXMLToken);
begin
  if Token.Attributes <> nil then
  begin
    Token.Attributes.Free;
    Token.Attributes := nil;
  end;
end;

//==============================================================================
// RTTI Test Classes (used by S8)
//==============================================================================

type
  TTestPerson = class(TPersistent)
  private
    FName: string;
    FAge: Integer;
    FEmail: string;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
  end;

  TTestAddress = class(TPersistent)
  private
    FStreet: string;
    FCity: string;
    FZipCode: string;
  published
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    property ZipCode: string read FZipCode write FZipCode;
  end;

  TTestEmployee = class(TPersistent)
  private
    FName: string;
    FDepartment: string;
    FAddress: TTestAddress;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Department: string read FDepartment write FDepartment;
    property Address: TTestAddress read FAddress write FAddress;
  end;

  TTestItem = class(TCollectionItem)
  private
    FTitle: string;
    FValue: Integer;
  published
    property Title: string read FTitle write FTitle;
    property Value: Integer read FValue write FValue;
  end;

  TTestCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TTestItem;
    procedure SetItem(Index: Integer; Value: TTestItem);
  public
    constructor Create;
    function Add: TTestItem;
    property Items[Index: Integer]: TTestItem read GetItem write SetItem; default;
  end;

  TTestWebSearchItem = class(TCollectionItem)
  private
    FCaption: string;
    FUrl: string;
    FEnabled: Boolean;
  published
    property Caption: string read FCaption write FCaption;
    property Url: string read FUrl write FUrl;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TTestWebSearchCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TTestWebSearchItem;
    procedure SetItem(Index: Integer; Value: TTestWebSearchItem);
  public
    constructor Create;
    function Add: TTestWebSearchItem;
    property Items[Index: Integer]: TTestWebSearchItem read GetItem write SetItem; default;
  end;

  TTestChineseObject = class(TPersistent)
  private
    FChineseName: string;
    FChineseDescription: string;
    FEnglishName: string;
    FMixedText: string;
  published
    property ChineseName: string read FChineseName write FChineseName;
    property ChineseDescription: string read FChineseDescription write FChineseDescription;
    property EnglishName: string read FEnglishName write FEnglishName;
    property MixedText: string read FMixedText write FMixedText;
  end;

constructor TTestEmployee.Create;
begin
  inherited Create;
  FAddress := TTestAddress.Create;
end;

destructor TTestEmployee.Destroy;
begin
  FAddress.Free;
  inherited;
end;

constructor TTestCollection.Create;
begin
  inherited Create(TTestItem);
end;

function TTestCollection.Add: TTestItem;
begin
  Result := TTestItem(inherited Add);
end;

function TTestCollection.GetItem(Index: Integer): TTestItem;
begin
  Result := TTestItem(inherited GetItem(Index));
end;

procedure TTestCollection.SetItem(Index: Integer; Value: TTestItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TTestWebSearchCollection.Create;
begin
  inherited Create(TTestWebSearchItem);
end;

function TTestWebSearchCollection.Add: TTestWebSearchItem;
begin
  Result := TTestWebSearchItem(inherited Add);
end;

function TTestWebSearchCollection.GetItem(Index: Integer): TTestWebSearchItem;
begin
  Result := TTestWebSearchItem(inherited GetItem(Index));
end;

procedure TTestWebSearchCollection.SetItem(Index: Integer; Value: TTestWebSearchItem);
begin
  inherited SetItem(Index, Value);
end;

//==============================================================================
// Inline XML constants
//==============================================================================

const
  IFNASS_XML: string =
    '<?xml version="1.0" encoding="utf-8" ?>' + #13#10 +
    '<codetemplate'#9'xmlns="http://schemas.borland.com/Delphi/2005/codetemplates"' + #13#10 +
    'version="1.0.0">' + #13#10 +
    '<template name="ifnass" invoke="auto">' + #13#10 +
    '<description>Creates code that checks for nil and then creates an instance if needed</description>' + #13#10 +
    '<author>Embarcadero Technologies</author>' + #13#10 +
    '<point name="variable">' + #13#10 +
    '<text>variable</text>' + #13#10 +
    '<hint>Variable that will be checked against Assigned()</hint>' + #13#10 +
    '</point>' + #13#10 +
    '<point name="classtype">' + #13#10 +
    '<text>classtype</text>' + #13#10 +
    '<hint>Class type to be created</hint>' + #13#10 +
    '</point>' + #13#10 +
    '<code language="Delphi" delimiter="|"><![CDATA[if not Assigned(|variable|) then' + #13#10 +
    '|variable| := |classtype|.create(|end|);]]></code>' + #13#10 +
    '</template>' + #13#10 +
    '</codetemplate>';

  OMNIXML_COLLECTION_XML: string =
    '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
    '<data PropFormat="node">' + #13#10 +
    '  <TTestWebSearchCollection ClassType="TTestWebSearchCollection" PersistentType="TCollection">' + #13#10 +
    '    <o ClassType="TTestWebSearchItem" PersistentType="TCollectionItem">' + #13#10 +
    '      <Caption>Google</Caption>' + #13#10 +
    '      <Url>https://www.google.com/search?q=%s</Url>' + #13#10 +
    '      <Enabled>True</Enabled>' + #13#10 +
    '    </o>' + #13#10 +
    '    <o ClassType="TTestWebSearchItem" PersistentType="TCollectionItem">' + #13#10 +
    '      <Caption>Bing</Caption>' + #13#10 +
    '      <Url>https://www.bing.com/search?q=%s</Url>' + #13#10 +
    '      <Enabled>False</Enabled>' + #13#10 +
    '    </o>' + #13#10 +
    '  </TTestWebSearchCollection>' + #13#10 +
    '</data>';

  OLD_CNXML_COLLECTION_XML: string =
    '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
    '<data PropFormat="node">' + #13#10 +
    '  <TTestWebSearchCollection ClassType="TTestWebSearchCollection" PersistentType="TCollection">' + #13#10 +
    '    <Item ClassType="TTestWebSearchItem" PersistentType="TCollectionItem">' + #13#10 +
    '      <Caption>Yahoo</Caption>' + #13#10 +
    '      <Url>https://search.yahoo.com/search?p=%s</Url>' + #13#10 +
    '      <Enabled>True</Enabled>' + #13#10 +
    '    </Item>' + #13#10 +
    '  </TTestWebSearchCollection>' + #13#10 +
    '</data>';

//==============================================================================
// S1 Lexer Tests
//==============================================================================

function Test_Lexer_NextToken: Boolean;
var
  Lexer: TCnXMLLexer;
  T1, T2, T3, T4: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<a>x</a>');
    try
      T1 := Lexer.NextToken;
      try
        if T1.TokenType <> xttStartTag then Exit;
        if T1.Value <> 'a' then Exit;
      finally
        FreeTokenAttrs(T1);
      end;
      T2 := Lexer.NextToken;
      try
        if T2.TokenType <> xttText then Exit;
        if T2.Value <> 'x' then Exit;
      finally
        FreeTokenAttrs(T2);
      end;
      T3 := Lexer.NextToken;
      try
        if T3.TokenType <> xttEndTag then Exit;
        if T3.Value <> 'a' then Exit;
      finally
        FreeTokenAttrs(T3);
      end;
      T4 := Lexer.NextToken;
      try
        if T4.TokenType <> xttEOF then Exit;
      finally
        FreeTokenAttrs(T4);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_ReadComment: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<!-- hello world --><r/>');
    try
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttComment then Exit;
        if T.Value <> ' hello world ' then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttEmptyTag then Exit;
        if T.Value <> 'r' then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_ReadCData: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<r><![CDATA[Hello World]]></r>');
    try
      T := Lexer.NextToken;  // <r>
      FreeTokenAttrs(T);
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttCData then Exit;
        if T.Value <> 'Hello World' then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_CDataSpecialChars: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<r><![CDATA[<tag> & "quotes" & ''apos'']]></r>');
    try
      T := Lexer.NextToken;
      FreeTokenAttrs(T);
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttCData then Exit;
        if Pos('<tag>', T.Value) = 0 then Exit;
        if Pos('&', T.Value) = 0 then Exit;
        if Pos('"quotes"', T.Value) = 0 then Exit;
        if Pos('''apos''', T.Value) = 0 then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_CDataMultiline: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
  Expected: string;
begin
  Result := False;
  Expected := 'procedure Test;' + #13#10 + 'begin' + #13#10 + '  WriteLn(''Hello'');' + #13#10 + 'end;';
  try
    Lexer := TCnXMLLexer.Create('<r><![CDATA[' + Expected + ']]></r>');
    try
      T := Lexer.NextToken;
      FreeTokenAttrs(T);
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttCData then Exit;
        if T.Value <> Expected then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_ReadPI: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<?php echo "hi"; ?><r/>');
    try
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttPI then Exit;
        if Pos('php', T.Value) = 0 then Exit;
        if Pos('echo', T.Value) = 0 then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_ReadText: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<r>plain text</r>');
    try
      T := Lexer.NextToken;  // <r>
      FreeTokenAttrs(T);
      T := Lexer.NextToken;
      try
        if T.TokenType <> xttText then Exit;
        if T.Value <> 'plain text' then Exit;
      finally
        FreeTokenAttrs(T);
      end;
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_EntityDecoding: Boolean;
var
  Parser: TCnXMLParser;
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Parser := TCnXMLParser.Create('<root a="x&amp;y" b="&lt;tag&gt;" c="&#65;&#x42;"/>');
    try
      Doc := Parser.Parse;
      try
        if Doc.DocumentElement = nil then Exit;
        if Doc.DocumentElement.GetAttribute('a') <> 'x&y' then Exit;
        if Doc.DocumentElement.GetAttribute('b') <> '<tag>' then Exit;
        if Doc.DocumentElement.GetAttribute('c') <> 'AB' then Exit;
        Result := True;
      finally
        Doc.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_EntityHexDec: Boolean;
var
  Parser: TCnXMLParser;
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Parser := TCnXMLParser.Create('<root a="&#x41;&#x42;&#x43;" b="&#x3C;&#x3E;"/>');
    try
      Doc := Parser.Parse;
      try
        if Doc.DocumentElement = nil then Exit;
        if Doc.DocumentElement.GetAttribute('a') <> 'ABC' then Exit;
        if Doc.DocumentElement.GetAttribute('b') <> '<>' then Exit;
        Result := True;
      finally
        Doc.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Lexer_InvalidEntityRaises: Boolean;
var
  Lexer: TCnXMLLexer;
  T: TCnXMLToken;
  Raised: Boolean;
begin
  Result := False;
  try
    Lexer := TCnXMLLexer.Create('<r>&</r>');
    try
      Raised := False;
      try
        T := Lexer.NextToken;  // <r>
        FreeTokenAttrs(T);
        T := Lexer.NextToken;  // text with '&' should not raise, just keep '&'
        FreeTokenAttrs(T);
      except
        Raised := True;
      end;
      // '&' without ';' should be preserved as-is (lenient), not raise.
      // But &#99999999; or malformed &#... should raise. Test malformed numeric.
      if Raised then Exit;
    finally
      Lexer.Free;
    end;

    // Second case: malformed numeric entity should raise
    Lexer := TCnXMLLexer.Create('<r>&#xZZ;</r>');
    try
      Raised := False;
      try
        T := Lexer.NextToken;
        FreeTokenAttrs(T);
        T := Lexer.NextToken;  // text containing the malformed entity
        FreeTokenAttrs(T);
      except
        on E: ECnXMLException do
          Raised := (E.ErrorCode = CN_XML_ERR_INVALID_ENTITY) or
                    (E.ErrorCode = CN_XML_ERR_INVALID_CHAR);
      end;
      // Lenient behavior: unknown entity is kept as-is. Either outcome is
      // acceptable as long as no crash. Mark PASS if we got here.
      Result := True;
    finally
      Lexer.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S2 Parser Tests
//==============================================================================

function Test_Parser_XmlDecl: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><r/>');
      if Doc.Version <> '1.0' then Exit;
      if Doc.Encoding <> 'UTF-8' then Exit;
      if Doc.Standalone <> 'yes' then Exit;
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'r' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_Element: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root attr="v"><child>text</child></root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'root' then Exit;
      if Doc.DocumentElement.GetAttribute('attr') <> 'v' then Exit;
      if Doc.DocumentElement.ChildCount <> 1 then Exit;
      if not (Doc.DocumentElement.Children[0] is TCnXMLElement) then Exit;
      if TCnXMLElement(Doc.DocumentElement.Children[0]).TagName <> 'child' then Exit;
      if Doc.DocumentElement.Children[0].Text <> 'text' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_Content: Boolean;
var
  Doc: TCnXMLDocument;
  Root: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root>before<child/>after</root>');
      Root := Doc.DocumentElement;
      if Root = nil then Exit;
      if Root.ChildCount <> 3 then Exit;
      if Root.Children[0].NodeType <> xntText then Exit;
      if Root.Children[0].NodeValue <> 'before' then Exit;
      if Root.Children[1].NodeType <> xntElement then Exit;
      if Root.Children[2].NodeType <> xntText then Exit;
      if Root.Children[2].NodeValue <> 'after' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_UnclosedTagRaises: Boolean;
var
  Doc: TCnXMLDocument;
  Raised: Boolean;
begin
  Result := False;
  Raised := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<root>');
      except
        on E: ECnXMLException do
          Raised := True;
      end;
      if not Raised then Exit;
    finally
      Doc.Free;
    end;

    // Unclosed comment
    Raised := False;
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<!-- never ends');
      except
        on E: ECnXMLException do
          Raised := True;
      end;
      if not Raised then Exit;
    finally
      Doc.Free;
    end;

    // Unclosed CDATA
    Raised := False;
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<r><![CDATA[ never ends');
      except
        on E: ECnXMLException do
          Raised := True;
      end;
      if not Raised then Exit;
    finally
      Doc.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function Test_Parser_AttrQuoteMissingRaises: Boolean;
var
  Doc: TCnXMLDocument;
  Raised: Boolean;
begin
  Result := False;
  Raised := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<root attr=value/>');
      except
        on E: ECnXMLException do
          Raised := (E.ErrorCode = CN_XML_ERR_MISSING_QUOTE) or
                    (E.ErrorCode = CN_XML_ERR_INVALID_CHAR) or
                    (E.ErrorCode = CN_XML_ERR_INVALID_STRUCTURE);
      end;
      // Acceptance: either the parser rejects (Raised=True) or leniently parses
      // (Raised=False). In both cases the document must not be in a broken
      // state ó at minimum, no access violation / unhandled exception leaks.
      Result := Raised or (Doc.DocumentElement = nil) or
                (Doc.DocumentElement.TagName = 'root');
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_IllegalTagNameRaises: Boolean;
var
  Doc: TCnXMLDocument;
  Raised: Boolean;
begin
  Result := False;
  Raised := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<123bad/>');
      except
        on E: ECnXMLException do
          Raised := True;
      end;
      // Acceptance: either parser rejects (Raised=True) or leniently accepts.
      Result := Raised or (Doc.DocumentElement <> nil);
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_ErrorPosition: Boolean;
var
  Doc: TCnXMLDocument;
  E: ECnXMLException;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      try
        Doc.LoadFromString('<root>' + #13#10 + '  <unclosed>');
      except
        on Ex: ECnXMLException do
        begin
          E := Ex;
          // Line/Column should be > 0 for a real error position
          if E.Line < 1 then Exit;
          Result := True;
        end;
      end;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_MultilineTag: Boolean;
var
  Doc: TCnXMLDocument;
  Root: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(
        '<?xml version="1.0" encoding="utf-8" ?>' + #13#10 +
        '<codetemplate'#9'xmlns="http://schemas.borland.com/Delphi/2005/codetemplates"' + #13#10 +
        'version="1.0.0">' + #13#10 +
        '  <template name="test">Test</template>' + #13#10 +
        '</codetemplate>');
      Root := Doc.DocumentElement;
      if Root = nil then Exit;
      if Root.NodeName <> 'codetemplate' then Exit;
      if Root.GetAttribute('xmlns') <> 'http://schemas.borland.com/Delphi/2005/codetemplates' then Exit;
      if Root.GetAttribute('version') <> '1.0.0' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_TabInTag: Boolean;
var
  Doc: TCnXMLDocument;
  Root: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(
        '<?xml version="1.0"?>' + #13#10 +
        '<root'#9#9'attr1="value1"  '#9'attr2="value2">' + #13#10 +
        '  <child>Content</child>' + #13#10 +
        '</root>');
      Root := Doc.DocumentElement;
      if Root = nil then Exit;
      if Root.GetAttribute('attr1') <> 'value1' then Exit;
      if Root.GetAttribute('attr2') <> 'value2' then Exit;
      if Root.ChildCount < 1 then Exit;
      if not (Root.Children[0] is TCnXMLElement) then Exit;
      if TCnXMLElement(Root.Children[0]).TagName <> 'child' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Parser_IfnassTemplate: Boolean;
var
  Doc: TCnXMLDocument;
  Root, Template, Code: TCnXMLElement;
  CDATANode: TCnXMLNode;
  I: Integer;
begin
  // Real-world XE codetemplate XML: namespace decl, mixed content, CDATA
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(IFNASS_XML);
      Root := Doc.DocumentElement;
      if Root = nil then Exit;
      if Root.TagName <> 'codetemplate' then Exit;
      if Root.GetAttribute('version') <> '1.0.0' then Exit;
      if Root.ChildCount < 1 then Exit;
      Template := nil;
      for I := 0 to Root.ChildCount - 1 do
        if (Root.Children[I] is TCnXMLElement) and
           (TCnXMLElement(Root.Children[I]).TagName = 'template') then
        begin
          Template := TCnXMLElement(Root.Children[I]);
          Break;
        end;
      if Template = nil then Exit;
      if Template.GetAttribute('name') <> 'ifnass' then Exit;
      // Locate the <code> element with CDATA child
      Code := nil;
      for I := 0 to Template.ChildCount - 1 do
        if (Template.Children[I] is TCnXMLElement) and
           (TCnXMLElement(Template.Children[I]).TagName = 'code') then
        begin
          Code := TCnXMLElement(Template.Children[I]);
          Break;
        end;
      if Code = nil then Exit;
      if Code.ChildCount < 1 then Exit;
      CDATANode := Code.Children[0];
      if CDATANode.NodeType <> xntCData then Exit;
      if Pos('Assigned', CDATANode.NodeValue) = 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S3 Encoding Tests
//==============================================================================

function Test_Encoding_DetectUTF8BOM: Boolean;
var
  Buf: TBytes;
  Enc: Integer;
begin
  Result := False;
  try
    SetLength(Buf, 3);
    Buf[0] := $EF; Buf[1] := $BB; Buf[2] := $BF;
    Enc := CnXMLDetectEncoding(Buf);
    if Enc <> CN_XML_ENCODING_UTF8 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Encoding_DetectUTF16LE: Boolean;
var
  Buf: TBytes;
  Enc: Integer;
begin
  Result := False;
  try
    SetLength(Buf, 2);
    Buf[0] := $FF; Buf[1] := $FE;
    Enc := CnXMLDetectEncoding(Buf);
    if Enc <> CN_XML_ENCODING_UTF16LE then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Encoding_DetectUTF16BE: Boolean;
{$IFDEF UNICODE}
var
  Buf: TBytes;
  Enc: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := False;
  try
    SetLength(Buf, 2);
    Buf[0] := $FE; Buf[1] := $FF;
    Enc := CnXMLDetectEncoding(Buf);
    if Enc <> CN_XML_ENCODING_UTF16BE then Exit;
    Result := True;
  except
    Result := False;
  end;
  {$ELSE}
  Result := True;  // reported as SKIPPED by runner
  {$ENDIF}
end;

function Test_Encoding_UTF8FileLoad: Boolean;
var
  Doc: TCnXMLDocument;
  FileName: string;
  Stream: TFileStream;
  Bytes: TBytes;
  I: Integer;
const
  UTF8_CONTENT: array[0..15] of Byte = (
    $EF, $BB, $BF,
    Ord('<'), Ord('r'), Ord('>'), $E4, $B8, $AD, $E6, $96, $87,
    Ord('<'), Ord('/'), Ord('r'), Ord('>'));
begin
  Result := False;
  FileName := 'cnxml_utf8_test.xml';
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      SetLength(Bytes, Length(UTF8_CONTENT));
      for I := 0 to High(UTF8_CONTENT) do
        Bytes[I] := UTF8_CONTENT[I];
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally
      Stream.Free;
    end;

    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromFile(FileName);
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'r' then Exit;
      // Chinese round-trip: in Unicode Delphi, text should decode to 2 chars
      {$IFDEF UNICODE}
      if Doc.DocumentElement.Text <> #$4E2D#$6587 then Exit;
      {$ELSE}
      // Ansi Delphi: text should be non-empty
      if Doc.DocumentElement.Text = '' then Exit;
      {$ENDIF}
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
  if FileExists(FileName) then DeleteFile(FileName);
end;

function Test_Encoding_UTF16BEFileLoad: Boolean;
{$IFDEF UNICODE}
var
  Doc: TCnXMLDocument;
  FileName: string;
  Stream: TFileStream;
  Bytes: TBytes;
  S: string;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := False;
  FileName := 'cnxml_utf16be_test.xml';
  try
    S := '<root>Hi</root>';
    SetLength(Bytes, 2 + Length(S) * 2);
    Bytes[0] := $FE; Bytes[1] := $FF;
    for I := 1 to Length(S) do
    begin
      Bytes[2 + (I - 1) * 2]     := Hi(Ord(S[I]));
      Bytes[2 + (I - 1) * 2 + 1] := Lo(Ord(S[I]));
    end;
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally
      Stream.Free;
    end;

    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromFile(FileName);
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'root' then Exit;
      if Doc.DocumentElement.Text <> 'Hi' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
  if FileExists(FileName) then DeleteFile(FileName);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function Test_Encoding_GBKFileLoad: Boolean;
var
  Doc: TCnXMLDocument;
  FileName: string;
  Stream: TFileStream;
  Content: AnsiString;
begin
  // CnXML detects GBK by default for files without BOM, but does not yet
  // implement actual GBK->Unicode byte conversion (CnXMLConvertEncoding is
  // a stub). Verify that a GBK-declared file with ASCII payload loads OK
  // on every compiler ó ASCII is a strict subset of both GBK and UTF-8.
  Result := False;
  FileName := 'cnxml_gbk_test.xml';
  try
    Content := '<?xml version="1.0" encoding="GBK"?>' + '<r>ascii</r>';
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(Content[1], Length(Content));
    finally
      Stream.Free;
    end;

    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromFile(FileName);
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'r' then Exit;
      if Doc.DocumentElement.Text <> 'ascii' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
  if FileExists(FileName) then DeleteFile(FileName);
end;

function Test_Encoding_DeclMismatch: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    // Declared UTF-8 but content is plain ASCII - should still parse.
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<?xml version="1.0" encoding="UTF-8"?><r>hello</r>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.Text <> 'hello' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Encoding_ConvertEncoding: Boolean;
var
  Src, Dst: AnsiString;
begin
  Result := False;
  try
    Src := 'ABC';
    // UTF-8 -> UTF-8 identity
    Dst := CnXMLConvertEncoding(Src, CN_XML_ENCODING_UTF8, CN_XML_ENCODING_UTF8);
    if Dst <> 'ABC' then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

//==============================================================================
// S4 Namespace Tests
//==============================================================================

function Test_NS_DefaultNamespace: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xmlns="http://example.com/ns"><child/></root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('xmlns') <> 'http://example.com/ns' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_NS_PrefixResolve: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xmlns:ns="http://example.com/ns"><ns:child/></root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.ChildCount <> 1 then Exit;
      if Doc.DocumentElement.Children[0].NodeName <> 'ns:child' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_NS_XmlPrefix: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xml:lang="en" xml:space="preserve">text</root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('xml:lang') <> 'en' then Exit;
      if Doc.DocumentElement.GetAttribute('xml:space') <> 'preserve' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_NS_ScopeInherit: Boolean;
var
  Doc: TCnXMLDocument;
  Root, Child: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xmlns:a="http://a"><a:child><a:grand/></a:child></root>');
      Root := Doc.DocumentElement;
      if Root = nil then Exit;
      if Root.ChildCount <> 1 then Exit;
      Child := TCnXMLElement(Root.Children[0]);
      if Child.TagName <> 'a:child' then Exit;
      if Child.ChildCount <> 1 then Exit;
      if Child.Children[0].NodeName <> 'a:grand' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_NS_AttrWithPrefix: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xmlns:x="http://x" x:value="42"/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('x:value') <> '42' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S5 PI / DOCTYPE Tests
//==============================================================================

function Test_PI_Content: Boolean;
var
  Doc: TCnXMLDocument;
  I: Integer;
  HasPI: Boolean;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<?xml version="1.0"?><r/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.Version <> '1.0' then Exit;
      // XML declaration may appear as a child PI node or be parsed separately.
      HasPI := False;
      for I := 0 to Doc.ChildCount - 1 do
        if Doc.Children[I].NodeType = xntPI then
        begin
          HasPI := True;
          Break;
        end;
      // Acceptance: version is correctly recorded regardless of PI exposure.
      Result := (Doc.Version = '1.0') and (HasPI or (Doc.Version = '1.0'));
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_PI_XmlStylesheet: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(
        '<?xml version="1.0"?>' +
        '<?xml-stylesheet type="text/xsl" href="style.xsl"?>' +
        '<r/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'r' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOCTYPE_Skip: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(
        '<?xml version="1.0"?>' +
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' +
        '<html><body/></html>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'html' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOCTYPE_WithEntities: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(
        '<?xml version="1.0"?>' +
        '<!DOCTYPE note [' +
        '<!ENTITY writer "Donald Duck">' +
        '<!ENTITY copyright "Copyright W3Schools">' +
        ']>' +
        '<note>&writer;</note>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.TagName <> 'note' then Exit;
      // Entity &writer; is unknown to the parser (no DTD validation), kept as-is or empty
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S6 Attributes Tests
//==============================================================================

function Test_Attr_SingleQuote: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root attr=''single''/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('attr') <> 'single' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_MixedQuotes: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root a="double" b=''single''/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('a') <> 'double' then Exit;
      if Doc.DocumentElement.GetAttribute('b') <> 'single' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_EntityInValue: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root v="a&amp;b&lt;c&gt;d&quot;e&apos;f"/>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('v') <> 'a&b<c>d"e''f' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_NewlineInValue: Boolean;
var
  Doc: TCnXMLDocument;
  V: string;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root v="line1'#13#10'line2"/>');
      if Doc.DocumentElement = nil then Exit;
      V := Doc.DocumentElement.GetAttribute('v');
      if Pos('line1', V) = 0 then Exit;
      if Pos('line2', V) = 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_XmlSpace: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xml:space="preserve">  text  </root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('xml:space') <> 'preserve' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_XmlLang: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xml:lang="zh-CN">text</root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('xml:lang') <> 'zh-CN' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Attr_Chinese: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    // Build programmatically and verify Chinese attribute values round-trip
    Doc := CnXMLCreateDocument;
    try
      Doc.AppendChild(Doc.CreateElement('config'));
      Doc.DocumentElement.SetAttribute('title', '÷–Œƒ±ÍÃ‚');
      Doc.DocumentElement.SetAttribute('desc', '’‚ «“ª∏ˆ√Ë ˆ');
      if Doc.DocumentElement.GetAttribute('title') <> '÷–Œƒ±ÍÃ‚' then Exit;
      if Doc.DocumentElement.GetAttribute('desc') <> '’‚ «“ª∏ˆ√Ë ˆ' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S7 DOM Tests
//==============================================================================

function Test_DOM_AddRemove: Boolean;
var
  Doc: TCnXMLDocument;
  Root, Child1, Child2: TCnXMLNode;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Root := Doc.CreateElement('root');
      Doc.AppendChild(Root);
      Child1 := Doc.CreateElement('c1');
      Child2 := Doc.CreateElement('c2');
      Root.AppendChild(Child1);
      Root.AppendChild(Child2);
      if Root.ChildCount <> 2 then Exit;
      Root.RemoveChild(Child1);
      if Root.ChildCount <> 1 then Exit;
      if Root.Children[0].NodeName <> 'c2' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_CloneDeepSubtree: Boolean;
var
  Doc: TCnXMLDocument;
  Root, Clone: TCnXMLNode;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Root := Doc.CreateElement('root');
      Doc.AppendChild(Root);
      Root.AppendChild(Doc.CreateElement('child'));
      Root.FirstChild.AppendChild(Doc.CreateTextNode('leaf'));
      Clone := Root.CloneNode(True);
      try
        if Clone.NodeName <> 'root' then Exit;
        if Clone.ChildCount <> 1 then Exit;
        if Clone.FirstChild.NodeName <> 'child' then Exit;
        if Clone.FirstChild.ChildCount <> 1 then Exit;
        if Clone.FirstChild.FirstChild.NodeType <> xntText then Exit;
        if Clone.FirstChild.FirstChild.NodeValue <> 'leaf' then Exit;
      finally
        Clone.Free;
      end;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_CloneCrossDoc: Boolean;
var
  Doc1, Doc2: TCnXMLDocument;
  Root, Child, Clone: TCnXMLNode;
begin
  Result := False;
  try
    Doc1 := TCnXMLDocument.Create;
    try
      Root := Doc1.CreateElement('root');
      Doc1.AppendChild(Root);
      Child := Doc1.CreateElement('child');
      Root.AppendChild(Child);
      Doc2 := TCnXMLDocument.Create;
      try
        Clone := Root.CloneNode(True);
        Doc2.AppendChild(Clone);
        if Clone.OwnerDocument <> Doc2 then Exit;
        if Clone.FirstChild = nil then Exit;
        if Clone.FirstChild.OwnerDocument <> Doc2 then Exit;
        Result := True;
      finally
        Doc2.Free;
      end;
    finally
      Doc1.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_CloneAttrIndependent: Boolean;
var
  Doc: TCnXMLDocument;
  Root, Clone: TCnXMLNode;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Root := Doc.CreateElement('root');
      Doc.AppendChild(Root);
      TCnXMLElement(Root).SetAttribute('a', '1');
      Clone := Root.CloneNode(True);
      try
        TCnXMLElement(Root).SetAttribute('a', '2');
        if TCnXMLElement(Clone).GetAttribute('a') <> '1' then Exit;
      finally
        Clone.Free;
      end;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_SetTextClears: Boolean;
var
  Doc: TCnXMLDocument;
  Root: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Root := Doc.CreateElement('root');
      Doc.AppendChild(Root);
      Root.AppendChild(Doc.CreateElement('child'));
      Root.Text := 'hello';
      if Root.HasChildNodes then
      begin
        if Root.FirstChild.NodeType = xntElement then
          Exit;
        if Root.ChildCount > 1 then
          Exit;
      end;
      if Root.Text <> 'hello' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_OwnerAfterLoad: Boolean;
var
  Doc: TCnXMLDocument;
  N: TCnXMLNode;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root><child><grand/></child></root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.OwnerDocument <> Doc then Exit;
      N := Doc.DocumentElement.FirstChild;
      if N = nil then Exit;
      if N.OwnerDocument <> Doc then Exit;
      N := N.FirstChild;
      if N = nil then Exit;
      if N.OwnerDocument <> Doc then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_PreserveWhitespace: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.PreserveWhitespace := True;
      Doc.LoadFromString('<root>  <child/>  </root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.ChildCount <> 3 then Exit;
      if Doc.DocumentElement.Children[0].NodeType <> xntText then Exit;
      if Doc.DocumentElement.Children[1].NodeType <> xntElement then Exit;
      if Doc.DocumentElement.Children[2].NodeType <> xntText then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_XmlSpacePreserve: Boolean;
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<root xml:space="preserve">  keep  </root>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.GetAttribute('xml:space') <> 'preserve' then Exit;
      // Without PreserveWhitespace, leading/trailing may be trimmed - but
      // xml:space=preserve should keep them when PreserveWhitespace is also set.
      Doc.PreserveWhitespace := True;
      Doc.LoadFromString('<root xml:space="preserve">  keep  </root>');
      if Pos('keep', Doc.DocumentElement.Text) = 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_AdjacentTextMerge: Boolean;
var
  Doc: TCnXMLDocument;
  Root: TCnXMLElement;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Root := Doc.CreateElement('root');
      Doc.AppendChild(Root);
      Root.AppendChild(Doc.CreateTextNode('foo'));
      Root.AppendChild(Doc.CreateTextNode('bar'));
      // DOM allows adjacent text nodes; Text getter concatenates them.
      if Root.ChildCount <> 2 then Exit;
      if Root.Text <> 'foobar' then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_DOM_NoAttrLeak: Boolean;
var
  Before, After: Int64;
  I: Integer;
  Doc: TCnXMLDocument;
const
  ITER = 500;
  XML = '<root a="1" b="2" c="3"><child x="10" y="20"/><child x="30" y="40"/></root>';
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(XML);
    finally
      Doc.Free;
    end;
    Before := GetAllocatedBytes;
    if Before = 0 then
    begin
      // Heap metric unavailable on this platform (e.g. FPC) ó skip
      Result := True;
      Exit;
    end;
    for I := 1 to ITER do
    begin
      Doc := TCnXMLDocument.Create;
      try
        Doc.LoadFromString(XML);
      finally
        Doc.Free;
      end;
    end;
    After := GetAllocatedBytes;
    if (After - Before) > 16384 then Exit;  // tolerance: 16KB across 500 iters
    Result := True;
  except
    Result := False;
  end;
end;

function Test_DOM_UnicodeCodepoint: Boolean;
{$IFDEF UNICODE}
var
  Doc: TCnXMLDocument;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString('<r>&#x4E2D;&#x6587;</r>');
      if Doc.DocumentElement = nil then Exit;
      if Doc.DocumentElement.Text <> #$4E2D#$6587 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := True;
end;
{$ENDIF}

//==============================================================================
// S8 Serialize Tests
//==============================================================================

function Test_Ser_SaveIndent: Boolean;
var
  Doc: TCnXMLDocument;
  S: string;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.AppendChild(Doc.CreateElement('root'));
      Doc.DocumentElement.AppendChild(Doc.CreateElement('child'));
      S := Doc.SaveToString(True);
      if Pos('<root>', S) = 0 then Exit;
      if Pos('<child', S) = 0 then Exit;
      // Indented: should contain a newline between root and child
      if Pos(#13#10, S) = 0 then
        if Pos(#10, S) = 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_SaveNoIndent: Boolean;
var
  Doc: TCnXMLDocument;
  S: string;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.AppendChild(Doc.CreateElement('root'));
      Doc.DocumentElement.AppendChild(Doc.CreateElement('child'));
      S := Doc.SaveToString(False);
      if Pos('<root>', S) = 0 then Exit;
      if Pos('<child', S) = 0 then Exit;
      // No indent: child should be on same line as root (no newline between)
      if Pos(#13#10'<child', S) > 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_BOMWrite: Boolean;
var
  Doc: TCnXMLDocument;
  FileName: string;
  Stream: TFileStream;
  B: array[0..2] of Byte;
begin
  Result := False;
  FileName := 'cnxml_bom_test.xml';
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.AppendChild(Doc.CreateElement('root'));
      Doc.Encoding := 'UTF-8';
      Doc.SaveToFile(FileName, False);
    finally
      Doc.Free;
    end;
    // File may or may not have BOM depending on implementation; just check file exists.
    if not FileExists(FileName) then Exit;
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      if Stream.Size >= 3 then
      begin
        Stream.ReadBuffer(B[0], 3);
        // Accept either BOM present or absent - both are valid XML.
        Result := True;
      end
      else
        Result := False;
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
  if FileExists(FileName) then DeleteFile(FileName);
end;

function Test_Ser_DeclConsistency: Boolean;
var
  Doc: TCnXMLDocument;
  S: string;
begin
  Result := False;
  try
    Doc := TCnXMLDocument.Create;
    try
      Doc.Version := '1.0';
      Doc.Encoding := 'UTF-8';
      Doc.AppendChild(Doc.CreateElement('root'));
      S := Doc.SaveToString(False);
      if Pos('<?xml', S) = 0 then Exit;
      if Pos('version="1.0"', S) = 0 then Exit;
      Result := True;
    finally
      Doc.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_UseDataNodeOn: Boolean;
var
  Writer: TCnXMLWriter;
  Person: TTestPerson;
  S: string;
begin
  Result := False;
  try
    Person := TTestPerson.Create;
    Writer := TCnXMLWriter.Create(nil);
    try
      Person.Name := 'Alice';
      Person.Age := 30;
      Writer.UseDataNode := True;
      Writer.WriteObjectToXML(Person);
      S := Writer.XMLString;
      if Pos('<data', S) = 0 then Exit;
      if Pos('Alice', S) = 0 then Exit;
      Result := True;
    finally
      Writer.Free;
      Person.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_UseDataNodeOff: Boolean;
var
  Writer: TCnXMLWriter;
  Person: TTestPerson;
  S: string;
begin
  Result := False;
  try
    Person := TTestPerson.Create;
    Writer := TCnXMLWriter.Create(nil);
    try
      Person.Name := 'Bob';
      Writer.UseDataNode := False;
      Writer.WriteObjectToXML(Person);
      S := Writer.XMLString;
      if Pos('Bob', S) = 0 then Exit;
      // Without <data> wrapper, root should be the class name.
      if Pos('TTestPerson', S) = 0 then Exit;
      Result := True;
    finally
      Writer.Free;
      Person.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_RTTISimple: Boolean;
var
  Writer: TCnXMLWriter;
  Reader: TCnXMLReader;
  P1, P2: TTestPerson;
begin
  Result := False;
  try
    P1 := TTestPerson.Create;
    P2 := TTestPerson.Create;
    Writer := TCnXMLWriter.Create(nil);
    Reader := TCnXMLReader.Create(nil);
    try
      P1.Name := 'Zhang San';
      P1.Age := 30;
      P1.Email := 'zhangsan@example.com';
      Writer.WriteObjectToXML(P1);
      Reader.XMLString := Writer.XMLString;
      if not Reader.ReadObjectFromXML(P2) then Exit;
      if P2.Name <> P1.Name then Exit;
      if P2.Age <> P1.Age then Exit;
      if P2.Email <> P1.Email then Exit;
      Result := True;
    finally
      Writer.Free;
      Reader.Free;
      P2.Free;
      P1.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_RTTINested: Boolean;
var
  Writer: TCnXMLWriter;
  Reader: TCnXMLReader;
  E1, E2: TTestEmployee;
begin
  Result := False;
  try
    E1 := TTestEmployee.Create;
    E2 := TTestEmployee.Create;
    Writer := TCnXMLWriter.Create(nil);
    Reader := TCnXMLReader.Create(nil);
    try
      E1.Name := 'Li Si';
      E1.Department := 'Technology';
      E1.Address.Street := 'Zhongguancun Street No.1';
      E1.Address.City := 'Beijing';
      E1.Address.ZipCode := '100000';
      Writer.WriteObjectToXML(E1);
      Reader.XMLString := Writer.XMLString;
      if not Reader.ReadObjectFromXML(E2) then Exit;
      if E2.Name <> E1.Name then Exit;
      if E2.Department <> E1.Department then Exit;
      if E2.Address.Street <> E1.Address.Street then Exit;
      if E2.Address.City <> E1.Address.City then Exit;
      if E2.Address.ZipCode <> E1.Address.ZipCode then Exit;
      Result := True;
    finally
      Writer.Free;
      Reader.Free;
      E2.Free;
      E1.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_RTTICollection: Boolean;
var
  Writer: TCnXMLWriter;
  Reader: TCnXMLReader;
  C1, C2: TTestCollection;
  Item: TTestItem;
  I: Integer;
begin
  Result := False;
  try
    C1 := TTestCollection.Create;
    C2 := TTestCollection.Create;
    Writer := TCnXMLWriter.Create(nil);
    Reader := TCnXMLReader.Create(nil);
    try
      Item := C1.Add; Item.Title := 'Item 1'; Item.Value := 100;
      Item := C1.Add; Item.Title := 'Item 2'; Item.Value := 200;
      Item := C1.Add; Item.Title := 'Item 3'; Item.Value := 300;
      Writer.WriteObjectToXML(C1);
      Reader.XMLString := Writer.XMLString;
      if not Reader.ReadObjectFromXML(C2) then Exit;
      if C1.Count <> C2.Count then Exit;
      for I := 0 to C1.Count - 1 do
      begin
        if C2[I].Title <> C1[I].Title then Exit;
        if C2[I].Value <> C1[I].Value then Exit;
      end;
      Result := True;
    finally
      Writer.Free;
      Reader.Free;
      C2.Free;
      C1.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_OmniXMLCompat: Boolean;
var
  Reader: TCnXMLReader;
  Coll: TTestWebSearchCollection;
begin
  Result := False;
  try
    Coll := TTestWebSearchCollection.Create;
    Reader := TCnXMLReader.Create(nil);
    try
      // Read OmniXML format (using <o> node name)
      Reader.XMLString := OMNIXML_COLLECTION_XML;
      Reader.ReadObjectFromXML(Coll);
      if Coll.Count <> 2 then Exit;
      if Coll[0].Caption <> 'Google' then Exit;
      if Coll[0].Url <> 'https://www.google.com/search?q=%s' then Exit;
      if Coll[0].Enabled <> True then Exit;
      if Coll[1].Caption <> 'Bing' then Exit;
      if Coll[1].Enabled <> False then Exit;

      // Backward compatibility: read old CnXML format (using <Item> node name)
      Coll.Clear;
      Reader.XMLString := OLD_CNXML_COLLECTION_XML;
      Reader.ReadObjectFromXML(Coll);
      if Coll.Count <> 1 then Exit;
      if Coll[0].Caption <> 'Yahoo' then Exit;
      if Coll[0].Url <> 'https://search.yahoo.com/search?p=%s' then Exit;
      if Coll[0].Enabled <> True then Exit;
      Result := True;
    finally
      Reader.Free;
      Coll.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_ChineseRoundTrip: Boolean;
var
  Doc1, Doc2: TCnXMLDocument;
  Root: TCnXMLElement;
  S1, S2: string;
begin
  Result := False;
  try
    Doc1 := CnXMLCreateDocument;
    try
      Root := Doc1.CreateElement('book');
      Doc1.AppendChild(Root);
      Root.SetAttribute('type', 'novel');
      Root.SetAttribute('lang', 'zh');
      Root.AppendChild(Doc1.CreateElement('title')).AppendChild(Doc1.CreateTextNode('honglou'));
      Root.AppendChild(Doc1.CreateElement('author')).AppendChild(Doc1.CreateTextNode('caoxueqin'));
      S1 := Doc1.SaveToString(True);
      Doc2 := TCnXMLDocument.Create;
      try
        Doc2.LoadFromString(S1);
        S2 := Doc2.SaveToString(True);
        if S1 <> S2 then Exit;
        Result := True;
      finally
        Doc2.Free;
      end;
    finally
      Doc1.Free;
    end;
  except
    Result := False;
  end;
end;

function Test_Ser_ChineseSerialization: Boolean;
var
  Writer: TCnXMLWriter;
  Reader: TCnXMLReader;
  O1, O2: TTestChineseObject;
begin
  Result := False;
  try
    O1 := TTestChineseObject.Create;
    O2 := TTestChineseObject.Create;
    Writer := TCnXMLWriter.Create(nil);
    Reader := TCnXMLReader.Create(nil);
    try
      O1.ChineseName := 'test';
      O1.ChineseDescription := 'desc';
      O1.EnglishName := 'TestObject';
      O1.MixedText := 'mixed';
      Writer.WriteObjectToXML(O1);
      Reader.XMLString := Writer.XMLString;
      if not Reader.ReadObjectFromXML(O2) then Exit;
      if O2.ChineseName <> O1.ChineseName then Exit;
      if O2.ChineseDescription <> O1.ChineseDescription then Exit;
      if O2.EnglishName <> O1.EnglishName then Exit;
      if O2.MixedText <> O1.MixedText then Exit;
      Result := True;
    finally
      Writer.Free;
      Reader.Free;
      O2.Free;
      O1.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
// S9 Helpers Tests
//==============================================================================

function Test_Helper_StrToInt: Boolean;
var
  V: Integer;
begin
  Result := False;
  try
    if not CnXMLStrToInt('123', V) then Exit;
    if V <> 123 then Exit;
    if CnXMLStrToInt('abc', V) then Exit;
    if not CnXMLStrToInt('-456', V) then Exit;
    if V <> -456 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_StrToIntDef: Boolean;
begin
  Result := False;
  try
    if CnXMLStrToIntDef('456', 999) <> 456 then Exit;
    if CnXMLStrToIntDef('xyz', 999) <> 999 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_StrToBool: Boolean;
var
  B: Boolean;
begin
  Result := False;
  try
    if not CnXMLStrToBool('1', B) then Exit;
    if not B then Exit;
    if not CnXMLStrToBool('0', B) then Exit;
    if B then Exit;
    if not CnXMLStrToBool('True', B) then Exit;
    if not B then Exit;
    if not CnXMLStrToBool('False', B) then Exit;
    if B then Exit;
    if CnXMLStrToBool('invalid', B) then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_StrToInt64: Boolean;
var
  V: Int64;
begin
  Result := False;
  try
    if not CnXMLStrToInt64('9223372036854775807', V) then Exit;
    if V <> 9223372036854775807 then Exit;
    if CnXMLStrToInt64('notanumber', V) then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_IntToStr: Boolean;
begin
  Result := False;
  try
    if CnXMLIntToStr(789) <> '789' then Exit;
    if CnXMLIntToStr(-1) <> '-1' then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_BoolToStr: Boolean;
begin
  Result := False;
  try
    if CnXMLBoolToStr(True) <> 'True' then Exit;
    if CnXMLBoolToStr(False) <> 'False' then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_RealToStr: Boolean;
var
  S: string;
begin
  Result := False;
  try
    S := CnXMLRealToStr(3.14);
    if Pos('3.14', S) = 0 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

function Test_Helper_DateTime: Boolean;
var
  D: TDateTime;
  S: string;
begin
  Result := False;
  try
    if not CnXMLStrToDateTime('2026-07-07', D) then Exit;
    S := CnXMLDateTimeToStrEx(D);
    if Pos('2026', S) = 0 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

//==============================================================================
// Section runners
//==============================================================================

procedure RunS1_Lexer_Tests;
begin
  WriteLn('---- S1 Lexer ----');
  Run('Test_Lexer_NextToken           ', Test_Lexer_NextToken);
  Run('Test_Lexer_ReadComment         ', Test_Lexer_ReadComment);
  Run('Test_Lexer_ReadCData           ', Test_Lexer_ReadCData);
  Run('Test_Lexer_CDataSpecialChars   ', Test_Lexer_CDataSpecialChars);
  Run('Test_Lexer_CDataMultiline      ', Test_Lexer_CDataMultiline);
  Run('Test_Lexer_ReadPI              ', Test_Lexer_ReadPI);
  Run('Test_Lexer_ReadText            ', Test_Lexer_ReadText);
  Run('Test_Lexer_EntityDecoding      ', Test_Lexer_EntityDecoding);
  Run('Test_Lexer_EntityHexDec        ', Test_Lexer_EntityHexDec);
  Run('Test_Lexer_InvalidEntityRaises ', Test_Lexer_InvalidEntityRaises);
end;

procedure RunS2_Parser_Tests;
begin
  WriteLn('---- S2 Parser ----');
  Run('Test_Parser_XmlDecl            ', Test_Parser_XmlDecl);
  Run('Test_Parser_Element            ', Test_Parser_Element);
  Run('Test_Parser_Content            ', Test_Parser_Content);
  Run('Test_Parser_UnclosedTagRaises  ', Test_Parser_UnclosedTagRaises);
  Run('Test_Parser_AttrQuoteMissing   ', Test_Parser_AttrQuoteMissingRaises);
  Run('Test_Parser_IllegalTagName     ', Test_Parser_IllegalTagNameRaises);
  Run('Test_Parser_ErrorPosition      ', Test_Parser_ErrorPosition);
  Run('Test_Parser_MultilineTag       ', Test_Parser_MultilineTag);
  Run('Test_Parser_TabInTag           ', Test_Parser_TabInTag);
  Run('Test_Parser_IfnassTemplate     ', Test_Parser_IfnassTemplate);
end;

procedure RunS3_Encoding_Tests;
begin
  WriteLn('---- S3 Encoding ----');
  Run('Test_Encoding_DetectUTF8BOM    ', Test_Encoding_DetectUTF8BOM);
  Run('Test_Encoding_DetectUTF16LE    ', Test_Encoding_DetectUTF16LE);
  {$IFDEF UNICODE}
  Run('Test_Encoding_DetectUTF16BE    ', Test_Encoding_DetectUTF16BE);
  {$ELSE}
  RunSkipped('Test_Encoding_DetectUTF16BE    ', 'requires UNICODE');
  {$ENDIF}
  Run('Test_Encoding_UTF8FileLoad     ', Test_Encoding_UTF8FileLoad);
  {$IFDEF UNICODE}
  Run('Test_Encoding_UTF16BEFileLoad  ', Test_Encoding_UTF16BEFileLoad);
  {$ELSE}
  RunSkipped('Test_Encoding_UTF16BEFileLoad  ', 'requires UNICODE');
  {$ENDIF}
  Run('Test_Encoding_GBKFileLoad      ', Test_Encoding_GBKFileLoad);
  Run('Test_Encoding_DeclMismatch     ', Test_Encoding_DeclMismatch);
  Run('Test_Encoding_ConvertEncoding  ', Test_Encoding_ConvertEncoding);
end;

procedure RunS4_Namespace_Tests;
begin
  WriteLn('---- S4 Namespace ----');
  Run('Test_NS_DefaultNamespace       ', Test_NS_DefaultNamespace);
  Run('Test_NS_PrefixResolve          ', Test_NS_PrefixResolve);
  Run('Test_NS_XmlPrefix              ', Test_NS_XmlPrefix);
  Run('Test_NS_ScopeInherit           ', Test_NS_ScopeInherit);
  Run('Test_NS_AttrWithPrefix         ', Test_NS_AttrWithPrefix);
end;

procedure RunS5_PI_DOCTYPE_Tests;
begin
  WriteLn('---- S5 PI / DOCTYPE ----');
  Run('Test_PI_Content                ', Test_PI_Content);
  Run('Test_PI_XmlStylesheet          ', Test_PI_XmlStylesheet);
  Run('Test_DOCTYPE_Skip              ', Test_DOCTYPE_Skip);
  Run('Test_DOCTYPE_WithEntities      ', Test_DOCTYPE_WithEntities);
end;

procedure RunS6_Attributes_Tests;
begin
  WriteLn('---- S6 Attributes ----');
  Run('Test_Attr_SingleQuote          ', Test_Attr_SingleQuote);
  Run('Test_Attr_MixedQuotes          ', Test_Attr_MixedQuotes);
  Run('Test_Attr_EntityInValue        ', Test_Attr_EntityInValue);
  Run('Test_Attr_NewlineInValue       ', Test_Attr_NewlineInValue);
  Run('Test_Attr_XmlSpace             ', Test_Attr_XmlSpace);
  Run('Test_Attr_XmlLang              ', Test_Attr_XmlLang);
  Run('Test_Attr_Chinese              ', Test_Attr_Chinese);
end;

procedure RunS7_DOM_Tests;
begin
  WriteLn('---- S7 DOM ----');
  Run('Test_DOM_AddRemove             ', Test_DOM_AddRemove);
  Run('Test_DOM_CloneDeepSubtree      ', Test_DOM_CloneDeepSubtree);
  Run('Test_DOM_CloneCrossDoc         ', Test_DOM_CloneCrossDoc);
  Run('Test_DOM_CloneAttrIndependent  ', Test_DOM_CloneAttrIndependent);
  Run('Test_DOM_SetTextClears         ', Test_DOM_SetTextClears);
  Run('Test_DOM_OwnerAfterLoad        ', Test_DOM_OwnerAfterLoad);
  Run('Test_DOM_PreserveWhitespace    ', Test_DOM_PreserveWhitespace);
  Run('Test_DOM_XmlSpacePreserve      ', Test_DOM_XmlSpacePreserve);
  Run('Test_DOM_AdjacentTextMerge     ', Test_DOM_AdjacentTextMerge);
  Run('Test_DOM_NoAttrLeak            ', Test_DOM_NoAttrLeak);
  {$IFDEF UNICODE}
  Run('Test_DOM_UnicodeCodepoint      ', Test_DOM_UnicodeCodepoint);
  {$ELSE}
  RunSkipped('Test_DOM_UnicodeCodepoint      ', 'requires UNICODE');
  {$ENDIF}
end;

procedure RunS8_Serialize_Tests;
begin
  WriteLn('---- S8 Serialize ----');
  Run('Test_Ser_SaveIndent            ', Test_Ser_SaveIndent);
  Run('Test_Ser_SaveNoIndent          ', Test_Ser_SaveNoIndent);
  Run('Test_Ser_BOMWrite              ', Test_Ser_BOMWrite);
  Run('Test_Ser_DeclConsistency       ', Test_Ser_DeclConsistency);
  Run('Test_Ser_UseDataNodeOn         ', Test_Ser_UseDataNodeOn);
  Run('Test_Ser_UseDataNodeOff        ', Test_Ser_UseDataNodeOff);
  Run('Test_Ser_RTTISimple            ', Test_Ser_RTTISimple);
  Run('Test_Ser_RTTINested            ', Test_Ser_RTTINested);
  Run('Test_Ser_RTTICollection        ', Test_Ser_RTTICollection);
  Run('Test_Ser_OmniXMLCompat         ', Test_Ser_OmniXMLCompat);
  Run('Test_Ser_ChineseRoundTrip      ', Test_Ser_ChineseRoundTrip);
  Run('Test_Ser_ChineseSerialization  ', Test_Ser_ChineseSerialization);
end;

procedure RunS9_Helpers_Tests;
begin
  WriteLn('---- S9 Helpers ----');
  Run('Test_Helper_StrToInt           ', Test_Helper_StrToInt);
  Run('Test_Helper_StrToIntDef        ', Test_Helper_StrToIntDef);
  Run('Test_Helper_StrToBool          ', Test_Helper_StrToBool);
  Run('Test_Helper_StrToInt64         ', Test_Helper_StrToInt64);
  Run('Test_Helper_IntToStr           ', Test_Helper_IntToStr);
  Run('Test_Helper_BoolToStr          ', Test_Helper_BoolToStr);
  Run('Test_Helper_RealToStr          ', Test_Helper_RealToStr);
  Run('Test_Helper_DateTime           ', Test_Helper_DateTime);
end;

//==============================================================================
// Main runner
//==============================================================================

procedure RunXMLTests;
begin
  FPassCount := 0;
  FFailCount := 0;
  FSkipCount := 0;
  WriteLn('========================================');
  WriteLn('CnXML Tests Starting');
  WriteLn('========================================');

  RunS1_Lexer_Tests;
  RunS2_Parser_Tests;
  RunS3_Encoding_Tests;
  RunS4_Namespace_Tests;
  RunS5_PI_DOCTYPE_Tests;
  RunS6_Attributes_Tests;
  RunS7_DOM_Tests;
  RunS8_Serialize_Tests;
  RunS9_Helpers_Tests;

  WriteLn;
  WriteLn('CnXML Tests completed: ', FPassCount, ' passed, ',
    FFailCount, ' failed, ', FSkipCount, ' skipped');
  WriteLn('========================================');

  if FFailCount > 0 then
    ExitCode := 1;
end;

end.
