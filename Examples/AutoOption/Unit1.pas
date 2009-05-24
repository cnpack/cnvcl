unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, CnAOTreeView, CnAutoOption, Spin, ComCtrls,
  Menus, Buttons, ExtCtrls;

type

{$M+} // Use $M+ to generate RTTI information. Or you can derive from TPersistent.

  TMyClass1 = class(TObject)
  private
    FBooleanVal: Boolean;
    FByteBoolVal: ByteBool;
    FDoubleVal: Double;
    FIntVal: Integer;
    FLongBoolVal: LongBool;
    FSingleVal: Single;
    FPassword: string;
    FStrVal: string;
    FWordBoolVal: WordBool;
  published
    property IntVal: Integer read FIntVal write FIntVal;
    property SingleVal: Single read FSingleVal write FSingleVal;
    property DoubleVal: Double read FDoubleVal write FDoubleVal;
    property BooleanVal: Boolean read FBooleanVal write FBooleanVal;
    property ByteBoolVal: ByteBool read FByteBoolVal write FByteBoolVal;
    property WordBoolVal: WordBool read FWordBoolVal write FWordBoolVal;
    property LongBoolVal: LongBool read FLongBoolVal write FLongBoolVal;
    property StrVal: string read FStrVal write FStrVal;
    property Password: string read FPassword write FPassword;
  end;

{$M-}

  TForm1 = class(TForm)
    Panel2: TPanel;
    pnlTreeView: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    HotKey1: THotKey;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Option: TCnOptionGroup;
    TreeView: TCnAOTreeView;
    FDate: TDate;
    FDateTime: TDateTime;
    FTime: TTime;
    FIntValue: Integer;
    FObj: TMyClass1;
    procedure OnCreateInplaceEdit(Sender: TObject; InplaceEdit: TControl;
      AOption: TCnBaseOption);
    procedure OnGetItemText(Sender: TObject; AOption: TCnOptionItem;
      var AText: string);
  public
    { Public declarations }
  published
    property DateTime: TDateTime read FDateTime write FDateTime;
    property Date: TDate read FDate write FDate;
    property Time: TTime read FTime write FTime;
    property IntValue: Integer read FIntValue write FIntValue;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDateTime := Now;
  FDate := Now;
  FTime := Now;
  FIntValue := 2;
  FObj := TMyClass1.Create;
  Option := TCnOptionGroup.Create(nil);
  Option.Text := 'AutoOption Demo';
  with Option.AddGroup('Application MainForm') do
  begin
    AddItem(Self, 'Caption');
    AddItem(Self, 'Height');
    AddItem(Self, 'Width');
    AddItem(Self, 'Align');
    AddItem(Self, 'Anchors', 'Anchors', 'Left'#13#10'Top'#13#10'Right'#13#10'Down');
    AddItem(Self, 'DateTime', 'DateTime');
    AddItem(Self, 'Date', 'Date');
    AddItem(Self, 'Time', 'Time');
    AddItem(Self, 'IntValue', 'Integer List', 'No.1'#13#10'No.2'#13#10'No.3');
    AddItem(Memo1, 'Lines', 'StringList');
  end;
  
  with Option.AddGroup('Label1 Control') do
  begin
    AddItem(Label1, 'Caption', 'Caption', 'Item1'#13#10'Item2'#13#10'Item3');
    AddItem(Label1, 'Hint');
    AddItem(Label1, 'ShowHint', 'Show Hint?');
    AddItem(Label1, 'Color');
    AddItem(Label1, 'Font');
    AddItem(Label1.Font, 'Style', 'Font Style');
  end;
  
  with Option.AddGroup('Checkbox1') do
  begin
    AddItem(CheckBox1, 'Checked');
    AddItem(CheckBox1, 'Caption');
    AddItem(CheckBox1, 'Enabled');
    AddItem(CheckBox1, 'Visible');
    AddItem(CheckBox1.Font, 'Style', 'Font Style');
  end;

  with Option.AddGroup('Button1') do
  begin
    AddItem(Button1, 'Caption');
    AddItem(Button1, 'Height');
    AddItem(Button1, 'Width');
  end;

  with Option.AddGroup('HotKey1') do
  begin
    AddItem(HotKey1, 'Hotkey');
    AddItem(HotKey1, 'Modifiers');
    AddItem(HotKey1, 'InvalidKeys');
  end;

  with Option.AddGroup('TMyClass1') do
  begin
    AddItem(FObj, 'IntVal', 'Integer', '', 10, 5, 20);
    AddItem(FObj, 'SingleVal', 'Single');
    AddItem(FObj, 'DoubleVal', 'Double');
    AddItem(FObj, 'StrVal', 'string');
    AddItem(FObj, 'Password', 'Password');
    AddItem(FObj, 'BooleanVal', 'Boolean');
    AddItem(FObj, 'ByteBoolVal', 'ByteBool');
    AddItem(FObj, 'WordBoolVal', 'WordBool');
    AddItem(FObj, 'LongBoolVal', 'LongBool');
  end;
    
  TreeView := TCnAOTreeView.Create(Self);
  TreeView.OnCreateInplaceEdit := OnCreateInplaceEdit;
  TreeView.OnGetItemText := OnGetItemText;
  TreeView.Parent := pnlTreeView;
  TreeView.Align := alClient;
  TreeView.Options := Option;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Option.Free;
  FObj.Free;
end;

procedure TForm1.OnCreateInplaceEdit(Sender: TObject;
  InplaceEdit: TControl; AOption: TCnBaseOption);
begin
  if (InplaceEdit is TCustomEdit) and (AOption is TCnOptionItem) and
    SameText(TCnOptionItem(AOption).PropName, 'Password') then
    TEdit(InplaceEdit).PasswordChar := '*';
end;

procedure TForm1.OnGetItemText(Sender: TObject; AOption: TCnOptionItem;
  var AText: string);
begin
  if SameText(TCnOptionItem(AOption).PropName, 'Password') then
    AText := '******';
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  TreeView.ApplyOption;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  TreeView.ResetOption;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  TreeView.DefaultOption;
end;

end.
