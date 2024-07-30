unit UnitJSON;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, TypInfo, CnJSON, CnCommon;

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
    tsJSONConstruct: TTabSheet;
    btnJSONConstruct1: TButton;
    mmoOutput: TMemo;
    chkConstructFormat: TCheckBox;
    btnWrite: TButton;
    btnComponent: TButton;
    btnGenHuge: TButton;
    dlgSave1: TSaveDialog;
    btnOpenHuge: TButton;
    dlgOpen1: TOpenDialog;
    btnJSONSort: TButton;
    btnClone: TButton;
    btnArray: TButton;
    btnMerge: TButton;
    chkReplaceName: TCheckBox;
    procedure btnParseClick(Sender: TObject);
    procedure btnJSONConstruct1Click(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure btnComponentClick(Sender: TObject);
    procedure btnGenHugeClick(Sender: TObject);
    procedure btnOpenHugeClick(Sender: TObject);
    procedure btnJSONSortClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure btnArrayClick(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
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
  CnWideStrings, CnSampleComponent;

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

  ShowMessage(IntToStr(Obj['animals']['dog'].Values[0]['age'].AsInteger));

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
        tvJSON.Items.AddChild(Pair, JObj.Names[I].AsString);
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

procedure TFormJSON.btnJSONConstruct1Click(Sender: TObject);
var
  JObj, JObj1: TCnJSONObject;
  JArr: TCnJSONArray;
  S, S1: AnsiString;
begin
  JObj := TCnJSONObject.Create;
  JArr := TCnJSONArray.Create;

  JObj.AddPair('Test1', 'Kick me');
  JObj.AddPair('Test2', True);
  JArr.AddValue('吃饭');
  JArr.AddValue('abc'#13#10'cde');
  JArr.AddValue(+5.56);
  JObj.AddPair('TestArray', JArr);

  JObj.AddPair('Test3', False);
  JObj.AddPair('Test4');
  JObj.AddPair('Test5', -323);
  JObj.AddPair('Test6', 3.14e8);

  S := JObj.ToJSON(chkConstructFormat.Checked);
{$IFDEF UNICODE}
  mmoOutput.Lines.Text := UTF8Decode(S);
{$ELSE}
  mmoOutput.Lines.Text := AnsiString(CnUtf8DecodeToWideString(S));
{$ENDIF}

  JObj1 := TCnJSONObject.Create;
  JObj1.Assign(JObj);

  S1 := JObj1.ToJSON(chkConstructFormat.Checked);
  ShowMessage(S1);

  if S1 = S then
    ShowMessage('Assign Equal')
  else
    ShowMessage('Assign Error');

  JObj1.Free;
  JObj.Free;
  // JArr 跟随 Free 了
end;

procedure TFormJSON.btnWriteClick(Sender: TObject);
begin
  mmoOutput.Lines.Text := TCnJSONWriter.SaveToJSON(Self);
end;

procedure TFormJSON.btnComponentClick(Sender: TObject);
var
  C: TCnSampleComponent;
  S: AnsiString;
begin
  C := TCnSampleComponent.Create(nil);
  mmoOutput.Lines.Text := TCnJSONWriter.SaveToJSON(C);

  TCnJSONReader.LoadFromJSON(C, mmoOutput.Lines.Text);

  S := TCnJSONWriter.SaveToJSON(C);
  if S <> mmoOutput.Lines.Text then
    ShowMessage('Error');
  C.Free;
end;

procedure TFormJSON.btnGenHugeClick(Sender: TObject);
var
  I: Integer;
  Obj: TCnJSONObject;
begin
  Obj := TCnJSONObject.Create;
  for I := 0 to 500000 do
    Obj.AddPair('TestName' + IntToStr(I), 'TestValue' + IntToStr(2 * I));

  if dlgSave1.Execute then
    TCnJSONWriter.JSONObjectToFile(Obj, dlgSave1.FileName);

  Obj.Free;
end;

procedure TFormJSON.btnOpenHugeClick(Sender: TObject);
var
  I, Cnt: Integer;
  Obj: TCnJSONObject;
  S: string;
  T, T1, T2: Cardinal;
begin
  if dlgOpen1.Execute then
  begin
    T := CnGetTickCount;
    Obj := TCnJSONReader.FileToJSONObject(dlgOpen1.FileName);
    T1 := CnGetTickCount;
    Cnt := 0;
    for I := 0 to 100 do
    begin
      S := Obj.ValueByName['TestName430950'].AsString;
      if S <> '' then
        Inc(Cnt);
    end;
    T2 := CnGetTickCount;
    ShowMessage(Format('Create %d ms. Search %d Times using %d ms', [T1 - T, Cnt, T2 - T1]));
  end;
end;

procedure TFormJSON.btnJSONSortClick(Sender: TObject);
var
  Obj: TCnJSONObject;
begin
  Obj := CnJSONParse(mmoOutput.Lines.Text);
  try
    Obj.Sort;
    mmoOutput.Lines.Text := Obj.ToJSON;
  finally
    Obj.Free;
  end;
end;

procedure TFormJSON.btnCloneClick(Sender: TObject);
var
  Obj, Dst: TCnJSONObject;
  S: AnsiString;
begin
  // 汉字转 UTF8
{$IFDEF UNICODE}
  S := Utf8Encode(mmoJSON.Lines.Text);
{$ELSE}
  S := CnUtf8EncodeWideString(WideString(mmoJSON.Lines.Text));
{$ENDIF}

  Obj := CnJSONParse(S);
  Dst := Obj.Clone as TCnJSONObject;
  S := Dst.ToJSON;

{$IFDEF UNICODE}
  mmoJSONReconstruct.Lines.Text := UTF8Decode(S);
{$ELSE}
  mmoJSONReconstruct.Lines.Text := AnsiString(CnUtf8DecodeToWideString(S));
{$ENDIF}
end;

procedure TFormJSON.btnArrayClick(Sender: TObject);
var
  Obj: TCnJSONObject;
  Arr: TCnJSONArray;
  V1: Integer;
  V2: Int64;
  V3: Extended;
  V4: Boolean;
  V5: TObject;
  V6: string;
  V7: WideString;
  V8: AnsiChar;
  V9: WideChar;
  V10: PAnsiChar;
  V11: PWideChar;
  S1: AnsiString;
  S2: WideString;
{$IFDEF UNICODE}
  V12: string;
{$ENDIF}
begin
  Obj := TCnJSONObject.Create;
  Arr := Obj.AddArray('Array');

  V1 := -123456;
  V2 := 987654321098765434;
  V3 := 1.59e-8;
  V4 := False;
  V5 := TCnJSONObject.Create;
  V6 := 'a string';
  V7 := 'a Wide String';
  V8 := #13;
  V9 := #$5403;  // '吃' 的 UTF16 编码

  S1 := 'a 吃饭的 PAnsiChar';
  S2 := 'A 喝水的 PWideChar';
  V10 := PAnsiChar(S1);
  V11 := PWideChar(S2);

{$IFDEF UNICODE}
  V12 := 'A 睡觉的Unicode String';
{$ENDIF}

  Arr.AddValues([V1, V2, V3, V4, V5, V6, V7, nil, V8, V9, V10, V11 {$IFDEF UNICODE}, V12 {$ENDIF}]);

{$IFDEF UNICODE}
  mmoJSONReconstruct.Lines.Text := UTF8Decode(Obj.ToJSON);
{$ELSE}
  mmoJSONReconstruct.Lines.Text := AnsiString(CnUtf8DecodeToWideString(Obj.ToJSON));
{$ENDIF}

  Obj.Free;
end;

procedure TFormJSON.btnMergeClick(Sender: TObject);
var
  Obj1, Obj2: TCnJSONObject;
  S1, S2: AnsiString;
  S: string;
begin
  // 汉字转 UTF8
  S := '{"animals":{"dog":[{"name":"Ru吃饭fus","age":15,"height":1.63},{"na":"Ma\u996drty","agee":null,"die":true}]}}';
{$IFDEF UNICODE}
  S1 := Utf8Encode(mmoJSON.Lines.Text);
  S2 := Utf8Encode(S);
{$ELSE}
  S1 := CnUtf8EncodeWideString(WideString(mmoJSON.Lines.Text));
  S2 := CnUtf8EncodeWideString(WideString(S));
{$ENDIF}

  Obj1 := CnJSONParse(S1);

  Obj2 := CnJSONParse(S2);

  CnJSONMergeObject(Obj1, Obj2);

  S2 := Obj2.ToJSON;
{$IFDEF UNICODE}
  mmoJSONReconstruct.Lines.Text := UTF8Decode(S2);
{$ELSE}
  mmoJSONReconstruct.Lines.Text := AnsiString(CnUtf8DecodeToWideString(S2));
{$ENDIF}

  Obj1.Free;
  Obj2.Free;
end;

end.
