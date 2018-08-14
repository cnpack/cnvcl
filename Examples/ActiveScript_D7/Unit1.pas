unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnClasses, CnActiveScript, CnASIDispatchProxy, StdCtrls;

type
  TForm1 = class(TForm)
    CnActiveScriptWindow1: TCnActiveScriptWindow;
    cbb1: TComboBox;
    lbl1: TLabel;
    lbl2: TLabel;
    mmo1: TMemo;
    btnRun: TButton;
    btnDemo1: TButton;
    btnDemo2: TButton;
    btnClose: TButton;
    btnExp: TButton;
    btnClean: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure btnExpClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
    procedure CnActiveScriptWindow1Error(Sender: TObject; Line,
      Pos: Integer; ASrc, ADescription: String);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemo1Click(Sender: TObject);
    procedure btnDemo2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnActiveScriptWindow1.CleanBeforeRun := False;
  
  GetActiveScriptParse(cbb1.Items);
  if cbb1.Items.Count > 0 then
    cbb1.Text := cbb1.Items[0];
end;

procedure TForm1.CnActiveScriptWindow1Error(Sender: TObject; Line,
  Pos: Integer; ASrc, ADescription: String);
begin
  Application.MessageBox(PChar(Format('Error Line: %d Pos: %d' + #13#10#13#10 +
    'Src: %s'#13#10'Description: %s', [Line, Pos, ASrc, ADescription])), 'Error',
    MB_OK + MB_ICONERROR);
end;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  CnActiveScriptWindow1.ScriptLanguage := cbb1.Text;
  CnActiveScriptWindow1.Execute(mmo1.Lines.Text);
end;

procedure TForm1.btnExpClick(Sender: TObject);
var
  Value: OleVariant;
begin
  CnActiveScriptWindow1.ScriptLanguage := cbb1.Text;
  Value := CnActiveScriptWindow1.RunExpression(mmo1.Lines.Text);
  if not VarIsNull(Value) then
    Application.MessageBox(PChar(string(Value)), 'Result', MB_OK);
end;

procedure TForm1.btnCleanClick(Sender: TObject);
begin
  CnActiveScriptWindow1.Clear;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnDemo1Click(Sender: TObject);
var
  s: WideString;
  v: OleVariant;
begin
  CnActiveScriptWindow1.Clear;
  CnActiveScriptWindow1.CleanBeforeRun := False;  // Must be False
  CnActiveScriptWindow1.ScriptLanguage := 'VBScript';
  s := // Declare a function
    'Function TestFunc()' + #13#10 +
    '  TestFunc = 12' + #13#10 +
    'End Function' + #13#10;
  CnActiveScriptWindow1.Execute(S);
  s := 'Func';  // Call this function
  v := CnActiveScriptWindow1.RunExpression('TestFunc');
  Application.MessageBox(PChar(string(v)), 'Call function', MB_OK);
end;

type
  ITest = interface(IActiveScriptInvokable)
    procedure MsgBox(Text: string); stdcall;
    procedure SetAppTitle(Text: string); stdcall;
    function Add(A, B: Integer): Integer; stdcall;
  end;

  TTest = class(TInterfacedObject, ITest)
  public
    destructor Destroy; override;
    procedure MsgBox(Text: string); stdcall;
    procedure SetAppTitle(Text: string); stdcall;
    function Add(A, B: Integer): Integer; stdcall;
  end;

{ TTest }

function TTest.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

destructor TTest.Destroy;
begin
  inherited;
end;

procedure TTest.MsgBox(Text: string);
begin
  Application.MessageBox(PChar(Text), '', MB_OK);
end;

procedure TTest.SetAppTitle(Text: string);
begin
  Application.Title := Text;
end;

procedure TForm1.btnDemo2Click(Sender: TObject);
var
  Intf: IUnknown;
begin
  Intf := GetIDispatchProxy(TTest.Create, TypeInfo(ITest));
  
  CnActiveScriptWindow1.Clear;
  CnActiveScriptWindow1.ScriptLanguage := 'VBScript';
  CnActiveScriptWindow1.AddNamedItem('Test', Intf);
  CnActiveScriptWindow1.Execute(
    'msgbox "Set application title => Test"' + #13#10 +
    'Test.SetAppTitle("Test")' + #13#10 +
    'Test.MsgBox("Test.Add(1, 3) = " & Test.Add(1, 3))' + #13#10 
    );
  CnActiveScriptWindow1.Clear; // ITest released
end;

end.
