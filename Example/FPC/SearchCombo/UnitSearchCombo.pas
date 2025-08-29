unit UnitSearchCombo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnSearchCombo, CnStrings, ComCtrls, ExtCtrls;

type
  TFormSearchCombo = class(TForm)
    grpBox: TGroupBox;
    btnShowBox: TButton;
    edtBox: TEdit;
    chkBoxCase: TCheckBox;
    rgBoxMatchMode: TRadioGroup;
    lblDrawIndent: TLabel;
    edtBoxIndent: TEdit;
    udBox: TUpDown;
    lblBoxSearch: TLabel;
    cbb1: TComboBox;
    cbb2: TComboBox;
    grpTestCombo: TGroupBox;
    chkComboCase: TCheckBox;
    lblComboSearch: TLabel;
    rgComboMatchMode: TRadioGroup;
    edtComboIndent: TEdit;
    lblComboDrawIndent: TLabel;
    udCombo: TUpDown;
    btnCreateCombo: TButton;
    lst1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnShowBoxClick(Sender: TObject);
    procedure edtBoxChange(Sender: TObject);
    procedure chkBoxCaseClick(Sender: TObject);
    procedure rgBoxMatchModeClick(Sender: TObject);
    procedure edtBoxIndentChange(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure btnCreateComboClick(Sender: TObject);
    procedure chkComboCaseClick(Sender: TObject);
    procedure rgComboMatchModeClick(Sender: TObject);
    procedure edtComboIndentChange(Sender: TObject);
    procedure lst1Click(Sender: TObject);
  private
    FBox: TCnDropDownBox;
    procedure ComboChange(Sender: TObject);
    procedure ComboSelect(Sender: TObject);
  public
    FCombo: TCnSearchComboBox;
  end;

var
  FormSearchCombo: TFormSearchCombo;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFormSearchCombo.FormCreate(Sender: TObject);
begin
  FBox := TCnDropDownBox.Create(Self);
  FBox.Parent := Self;
end;

procedure TFormSearchCombo.btnShowBoxClick(Sender: TObject);
begin
  if FBox.Visible then
  begin
    FBox.Hide;
    FBox.Clear;
  end
  else
  begin
    FBox.Items.Add('TestMe1');
    FBox.Items.Add('EatMe2');
    FBox.Items.Add('EatCake3');
    FBox.Items.Add('4PlayBall5');

    FBox.UpdateDisplay;
    FBox.Popup;
  end;
end;

procedure TFormSearchCombo.edtBoxChange(Sender: TObject);
begin
  FBox.MatchStr := edtBox.Text;
end;

procedure TFormSearchCombo.chkBoxCaseClick(Sender: TObject);
begin
  FBox.CaseSensitive := chkBoxCase.Checked;
end;

procedure TFormSearchCombo.rgBoxMatchModeClick(Sender: TObject);
begin
  FBox.MatchMode := TCnMatchMode(rgBoxMatchMode.ItemIndex);  
end;

procedure TFormSearchCombo.edtBoxIndentChange(Sender: TObject);
begin
  FBox.Indent := StrToInt(edtBoxIndent.Text);
end;

procedure TFormSearchCombo.cbb1Change(Sender: TObject);
begin
  Caption := DateTimeToStr(Now);
end;

procedure TFormSearchCombo.btnCreateComboClick(Sender: TObject);
begin
  if FCombo = nil then
  begin
    FCombo := TCnSearchComboBox.Create(Self);
    FCombo.Parent := grpTestCombo;
    FCombo.Left := 80;
    FCombo.Top := 72;

    FCombo.Items.Add('CnWizNotifierServices');
    FCombo.Items.Add('TWMDrawClipboard');
    FCombo.Items.Add('_LStrFromWStr');
    FCombo.Items.Add('CnStringReplaceA');
    FCombo.Items.Add('TCnObjectRingBuffer');
    FCombo.Items.Add('UnregisterHotKey');
    FCombo.Items.Add('EWX_FORCEIFHUNG');
    FCombo.Items.Add('csNoDesignVisible');
    FCombo.Items.Add('WantChildKey');
    FCombo.Items.Add('InitInheritedComponent');
    FCombo.Items.Add('BigNumberDirectMulMod');
    FCombo.Items.Add('CnInt64GenerateGaloisDivisionPolynomials');
    FCombo.Items.Add('CnEccRecoverPublicKeyFromFile');
    FCombo.Items.Add('Int64AffinePointToEccPoint');
    FCombo.Items.Add('_DynArrayLength');

    FCombo.OnChange := ComboChange;
    FCombo.OnSelect := ComboSelect;
  end;
end;

procedure TFormSearchCombo.chkComboCaseClick(Sender: TObject);
begin
  if FCombo <> nil then
    FCombo.CaseSensitive := chkComboCase.Checked;
end;

procedure TFormSearchCombo.rgComboMatchModeClick(Sender: TObject);
begin
  if FCombo <> nil then
    FCombo.MatchMode := TCnMatchMode(rgComboMatchMode.ItemIndex);
end;

procedure TFormSearchCombo.edtComboIndentChange(Sender: TObject);
begin
  if FCombo <> nil then
    FCombo.Indent := StrToInt(edtComboIndent.Text);
end;

procedure TFormSearchCombo.lst1Click(Sender: TObject);
begin
  Caption := DateTimeToStr(Now);
end;

procedure TFormSearchCombo.ComboChange(Sender: TObject);
begin
  if FCombo <> nil then
    Caption := DateTimeToStr(Now) + ' ' + FCombo.Text;
end;

procedure TFormSearchCombo.ComboSelect(Sender: TObject);
begin
  if FCombo <> nil then
    Caption := DateTimeToStr(Now) + ' Select: ' + FCombo.Text;
end;

end.
