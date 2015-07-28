unit UnitMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnMemo, CnSpin;

type
  TCnMemoForm = class(TForm)
    chkShowLineNumber: TCheckBox;
    chkHilightLineNumber: TCheckBox;
    btnChangeFont: TButton;
    dlgFontMemo: TFontDialog;
    lblLeftMargin: TLabel;
    seLeftMargin: TCnSpinEdit;
    lblRightMargin: TLabel;
    seRightMargin: TCnSpinEdit;
    grpColors: TGroupBox;
    btnLineBkColor: TButton;
    btnLineNumberColor: TButton;
    btnLineNumberHighlight: TButton;
    dlgColor: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure chkShowLineNumberClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure chkHilightLineNumberClick(Sender: TObject);
    procedure seLeftMarginChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure btnLineBkColorClick(Sender: TObject);
    procedure btnLineNumberColorClick(Sender: TObject);
    procedure btnLineNumberHighlightClick(Sender: TObject);
  private
    { Private declarations }
    FMemo: TCnMemo;
  public
    { Public declarations }
  end;

var
  CnMemoForm: TCnMemoForm;

implementation

{$R *.DFM}

procedure TCnMemoForm.FormCreate(Sender: TObject);
begin
  FMemo := TCnMemo.Create(Self);
  FMemo.Left := 16;
  FMemo.Top := 48;
  FMemo.Width := 300;
  FMemo.Height := 200;
  FMemo.Anchors := [akLeft, akRight, akTop, akBottom];

  FMemo.Parent := Self;
  seLeftMargin.Value := FMemo.LineNumberLeftMargin;
  seRightMargin.Value := FMemo.LineNumberRightMargin;
end;

procedure TCnMemoForm.chkShowLineNumberClick(Sender: TObject);
begin
  FMemo.ShowLineNumber := chkShowLineNumber.Checked;
end;

procedure TCnMemoForm.btnChangeFontClick(Sender: TObject);
begin
  if dlgFontMemo.Execute then
    FMemo.Font := dlgFontMemo.Font;
end;

procedure TCnMemoForm.chkHilightLineNumberClick(Sender: TObject);
begin
  FMemo.HighlightNumber := chkHilightLineNumber.Checked;
end;

procedure TCnMemoForm.seLeftMarginChange(Sender: TObject);
begin
  FMemo.LineNumberLeftMargin := seLeftMargin.Value;
end;

procedure TCnMemoForm.seRightMarginChange(Sender: TObject);
begin
  FMemo.LineNumberRightMargin := seRightMargin.Value;
end;

procedure TCnMemoForm.btnLineBkColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.LineNumberBkColor := dlgColor.Color;
end;

procedure TCnMemoForm.btnLineNumberColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.LineNumberColor := dlgColor.Color;
end;

procedure TCnMemoForm.btnLineNumberHighlightClick(Sender: TObject);
begin
  if dlgColor.Execute then
    FMemo.HighlightNumberColor := dlgColor.Color;
end;

end.
