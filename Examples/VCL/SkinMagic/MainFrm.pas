unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  CnSkinMagic, CnSkinMagic_Sample, StdCtrls, ExtCtrls, Mask, Buttons, DBCtrls,
  ShellAPI, CnClasses;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Button5: TButton;
    DBEdit1: TDBEdit;
    DBMemo1: TDBMemo;
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RadioGroup1: TRadioGroup;
    Memo1: TMemo;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    MaskEdit1: TMaskEdit;
    Button4: TButton;
    BitBtn1: TBitBtn;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    GroupBox3: TGroupBox;
    SpeedButton1: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    SpeedButton2: TSpeedButton;
    Shape1: TShape;
    Button6: TButton;
    CnSkinMagic1: TCnSkinMagic;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CnSkinMagic1.EnableSkin;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  CnSkinMagic1.DisableSkin;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  with TButton.Create(Self) do
  begin
    Left := 20;
    Top := 20;
    Width := 100;
    Parent := Panel2;
    Caption := 'Runtime Button';
  end;

  with TEdit.Create(Self) do
  begin
    Left := 20;
    Top := 50;
    Width := 100;
    Parent := Panel2;
    Text := 'Runtime Edit';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Font := Screen.IconFont;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.cnpack.org', '', '', SW_NORMAL);
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  TMainForm.Create(Application).Show;
end;

end.
