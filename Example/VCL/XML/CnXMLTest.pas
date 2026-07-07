unit CnXMLTest;
{* |<PRE>
================================================================================
* Software Name: CnPack Component Package
* Unit Name: CnXML Bugfix Test Unit
* Purpose: Regression tests for bugs found in CnXML.pas review.
*          Each test is designed to FAIL before the fix and PASS after.
*          Covers P0 (correctness/memory) and P1 (spec compliance) issues.
*
* Development Platform: PWin7 + Delphi 2009+
* Compatible Platform: PWin7/10 + Delphi 5+, FPC
* Note: ASCII-only comments to keep file encoding portable.
* Version History: 2026.07.07 V1.0
*                  Created unit
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnXML;

// P0: Attribute values must decode entity references (&amp; &lt; &#NN; etc.)
function TestAttributeEntityDecoding: Boolean;

// P0: Parsing many tokens with attributes must not leak TStringList instances
//     (TCnXMLToken.Attributes leak through record copy in TCnXMLParser.NextToken)
function TestNoAttributeLeak: Boolean;

// P0: LoadFromString with UTF-16 BE BOM must skip the BOM after byte-swap
function TestUTF16BEBOMString: Boolean;

// P0: After LoadFromString, all descendant nodes' OwnerDocument must point to
//     the new document (current code only updates the root element)
function TestOwnerDocumentAfterLoad: Boolean;

// P0: Element.SetText must clear existing element children (DOM semantics)
function TestSetTextClearsElementChildren: Boolean;

// P1: Unclosed comment must raise ECnXMLException with UNEXPECTED_EOF code
function TestUnclosedCommentRaises: Boolean;

// P1: Unclosed CDATA must raise ECnXMLException with UNEXPECTED_EOF code
function TestUnclosedCDataRaises: Boolean;

// P1: PreserveWhitespace=True must keep whitespace-only text nodes
function TestPreserveWhitespaceFlag: Boolean;

// P1: CloneNode(True) appended to another document must update OwnerDocument
//     for the entire cloned subtree
function TestCloneOwnerDocumentConsistency: Boolean;

procedure RunXMLTests;

implementation

//==============================================================================
// Helper: portable allocated-bytes snapshot for leak detection
//==============================================================================

function GetAllocatedBytes: Int64;
{$IFDEF FPC}
var
  H: THeapStatus;
begin
  H := GetHeapStatus;
  Result := H.CurrentHeapSize - H.CurrentHeapFree;
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
  Inc(Result, State.TotalAllocatedMediumBlock);
  Inc(Result, State.TotalAllocatedLargeBlock);
end;
  {$ELSE}
// Pre-Unicode Delphi (D5-D2007): use GetHeapStatus (THeapStatus)
var
  H: THeapStatus;
begin
  H := GetHeapStatus;
  Result := H.TotalAllocated;
end;
  {$ENDIF}
{$ENDIF}

//==============================================================================
// P0: Attribute entity decoding
//==============================================================================

function TestAttributeEntityDecoding: Boolean;
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

//==============================================================================
// P0: No TStringList leak from TCnXMLToken.Attributes
//==============================================================================

function TestNoAttributeLeak: Boolean;
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
    // Warm up allocator
    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(XML);
    finally
      Doc.Free;
    end;

    Before := GetAllocatedBytes;
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

    // Each iteration leaks 3 TStringLists (root + 2 children with attrs)
    // before fix. Allow 8KB slack for allocator fragmentation.
    if (After - Before) > 8192 then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

//==============================================================================
// P0: UTF-16 BE BOM handling in LoadFromString (Unicode Delphi only)
//==============================================================================

function TestUTF16BEBOMString: Boolean;
{$IFDEF UNICODE}
var
  Doc: TCnXMLDocument;
  S: string;
  I: Integer;
begin
  Result := False;
  try
    // Build a UTF-16 BE encoded string: BOM (FFFE) + byte-swapped chars
    S := '<root>Hi</root>';
    S := #$FFFE + S;
    for I := 2 to Length(S) do
      S[I] := Char(Swap(Ord(S[I])));

    Doc := TCnXMLDocument.Create;
    try
      Doc.LoadFromString(S);
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
end;
{$ELSE}
begin
  // Non-Unicode Delphi/FPC: string is AnsiString, UTF-16 BE path not applicable
  Result := True;
end;
{$ENDIF}

//==============================================================================
// P0: OwnerDocument consistency after LoadFromString
//==============================================================================

function TestOwnerDocumentAfterLoad: Boolean;
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
    // AV from dangling OwnerDocument pointer counts as failure
    Result := False;
  end;
end;

//==============================================================================
// P0: SetText clears element children
//==============================================================================

function TestSetTextClearsElementChildren: Boolean;
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

      // After SetText, no element child should remain
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

//==============================================================================
// P1: Unclosed comment raises UNEXPECTED_EOF
//==============================================================================

function TestUnclosedCommentRaises: Boolean;
var
  Parser: TCnXMLParser;
  Doc: TCnXMLDocument;
  ErrorRaised: Boolean;
begin
  Result := False;
  ErrorRaised := False;
  try
    Parser := TCnXMLParser.Create('<root><!-- unclosed comment');
    try
      try
        Doc := Parser.Parse;
        Doc.Free;
      except
        on E: ECnXMLException do
          ErrorRaised := (E.ErrorCode = CN_XML_ERR_UNEXPECTED_EOF);
      end;
    finally
      Parser.Free;
    end;
    if not ErrorRaised then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

//==============================================================================
// P1: Unclosed CDATA raises UNEXPECTED_EOF
//==============================================================================

function TestUnclosedCDataRaises: Boolean;
var
  Parser: TCnXMLParser;
  Doc: TCnXMLDocument;
  ErrorRaised: Boolean;
begin
  Result := False;
  ErrorRaised := False;
  try
    Parser := TCnXMLParser.Create('<root><![CDATA[unclosed');
    try
      try
        Doc := Parser.Parse;
        Doc.Free;
      except
        on E: ECnXMLException do
          ErrorRaised := (E.ErrorCode = CN_XML_ERR_UNEXPECTED_EOF);
      end;
    finally
      Parser.Free;
    end;
    if not ErrorRaised then Exit;
    Result := True;
  except
    Result := False;
  end;
end;

//==============================================================================
// P1: PreserveWhitespace flag keeps whitespace-only text nodes
//==============================================================================

function TestPreserveWhitespaceFlag: Boolean;
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
      // With PreserveWhitespace=True, expect 3 children: text, element, text
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

//==============================================================================
// P1: CloneNode subtree OwnerDocument consistency
//==============================================================================

function TestCloneOwnerDocumentConsistency: Boolean;
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

//==============================================================================
// Runner
//==============================================================================

type
  TTestFunc = function: Boolean;

procedure RunXMLTests;
var
  PassCount, FailCount: Integer;

  procedure Run(const Name: string; TestFunc: TTestFunc);
  begin
    Write(Name, ': ');
    if TestFunc then
    begin
      WriteLn('PASS');
      Inc(PassCount);
    end
    else
    begin
      WriteLn('FAIL');
      Inc(FailCount);
    end;
  end;

begin
  PassCount := 0;
  FailCount := 0;
  WriteLn('========================================');
  WriteLn('CnXML Tests Starting');
  WriteLn('========================================');

  Run('TestAttributeEntityDecoding     ', TestAttributeEntityDecoding);
  Run('TestNoAttributeLeak             ', TestNoAttributeLeak);
  Run('TestUTF16BEBOMString            ', TestUTF16BEBOMString);
  Run('TestOwnerDocumentAfterLoad      ', TestOwnerDocumentAfterLoad);
  Run('TestSetTextClearsElementChildren', TestSetTextClearsElementChildren);
  Run('TestUnclosedCommentRaises       ', TestUnclosedCommentRaises);
  Run('TestUnclosedCDataRaises         ', TestUnclosedCDataRaises);
  Run('TestPreserveWhitespaceFlag      ', TestPreserveWhitespaceFlag);
  Run('TestCloneOwnerDocumentConsistency', TestCloneOwnerDocumentConsistency);

  WriteLn;
  WriteLn('CnXML Tests completed: ', PassCount, ' passed, ', FailCount, ' failed');
  WriteLn('========================================');

  if FailCount > 0 then
    ExitCode := 1;
end;

end.
