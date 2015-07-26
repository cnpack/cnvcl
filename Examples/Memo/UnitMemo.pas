unit UnitMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnMemo;

type
  TCnMemoForm = class(TForm)
    chkShowLineNumber: TCheckBox;
    chkHilightLineNumber: TCheckBox;
    btnChangeFont: TButton;
    dlgFontMemo: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure chkShowLineNumberClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure chkHilightLineNumberClick(Sender: TObject);
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

end.
