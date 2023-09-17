unit UnitJSON;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, TypInfo, CnJSON;

type
  TFormJSON = class(TForm)
    pgc1: TPageControl;
    tsJSONParse: TTabSheet;
    mmoJSONReconstruct: TMemo;
    lblReconstruct: TLabel;
    chkJSONFormat: TCheckBox;
    tvJSON: TTreeView;
    mmoJSON: TMemo;
    btnParse: TButton;
    mmoJSONToken: TMemo;
    lblToken: TLabel;
    lblStr: TLabel;
    procedure btnParseClick(Sender: TObject);
  private
    procedure DumpJSONToTreeView(JSON: TCnJSONObject);
  public
    { Public declarations }
  end;

var
  FormJSON: TFormJSON;

implementation

{$R *.dfm}

uses
  CnWideStrings;

procedure TFormJSON.btnParseClick(Sender: TObject);
var
  Parser: TCnJSONParser;
  S: AnsiString;
  I: Integer;
  Obj: TCnJSONObject;
begin
  Parser := TCnJSONParser.Create;

  // 汉字转 UTF8
{$IFDEF UNICODE}
  S := Utf8Encode(mmoJSON.Lines.Text);
{$ELSE}
  S := CnUtf8EncodeWideString(WideString(mmoJSON.Lines.Text));
{$ENDIF}
  Parser.Origin := PAnsiChar(S);

  I := 1;
  mmoJSONToken.Lines.Clear;
  while Parser.TokenID <> jttTerminated do
  begin
    if Parser.TokenID = jttBlank then
      mmoJSONToken.Lines.Add(Format('%3.3d. Length %3.3d, Pos %4.4d. %s, Token: <blank>',
        [I, Parser.TokenLength, Parser.RunPos, GetEnumName(TypeInfo(TCnJSONTokenType),
         Ord(Parser.TokenID))]))
    else
      mmoJSONToken.Lines.Add(Format('%3.3d. Length %3.3d, Pos %4.4d. %s, Token: %s',
        [I, Parser.TokenLength, Parser.RunPos, GetEnumName(TypeInfo(TCnJSONTokenType),
         Ord(Parser.TokenID)), Parser.Token]));
    Parser.Next;
    Inc(I);
  end;

  Obj := CnJSONParse(S);
  DumpJSONToTreeView(Obj);
  S := Obj.ToJSON(chkJSONFormat.Checked); // UTF8 格式，不能直接塞到 Memo 里

{$IFDEF UNICODE}
  mmoJSONReconstruct.Lines.Text := UTF8Decode(S);
{$ELSE}
  mmoJSONReconstruct.Lines.Text := AnsiString(CnUtf8DecodeToWideString(S));
{$ENDIF}

  Obj.Free;
  Parser.Free;
end;

procedure TFormJSON.DumpJSONToTreeView(JSON: TCnJSONObject);

  procedure AddJSONValueToNode(Value: TCnJSONValue; ParentNode: TTreeNode);
  var
    I: Integer;
    Node, Pair: TTreeNode;
    JObj: TCnJSONObject;
    JArray: TCnJSONArray;
  begin
    if Value is TCnJSONObject then
    begin
      Node := tvJSON.Items.AddChild(ParentNode, '<object>');          // Node 是当前 object
      JObj := Value as TCnJSONObject;
      for I := 0 to JObj.Count - 1 do
      begin
        Pair := tvJSON.Items.AddChild(Node, Format('<pair:%d>', [I])); // Node 是每个 Pair 的 Parent
        tvJSON.Items.AddChild(Pair, JObj.Names[I].Content);
        AddJSONValueToNode(JObj.Values[I], Pair);
      end;
    end
    else if Value is TCnJSONArray then
    begin
      Node := tvJSON.Items.AddChild(ParentNode, '<array>');           // Node 是当前 array
      JArray := Value as TCnJSONArray;
      for I := 0 to JArray.Count - 1 do
        AddJSONValueToNode(JArray.Values[I], Node);
    end
    else if Value is TCnJSONString then
      tvJSON.Items.AddChild(ParentNode, (Value as TCnJSONString).AsString)
    else
      tvJSON.Items.AddChild(ParentNode, Value.Content);
  end;

begin
  tvJSON.Items.Clear;
  AddJSONValueToNode(JSON, nil);
  tvJSON.Items[0].Expand(True);
end;

end.
