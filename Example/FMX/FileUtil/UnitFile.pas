unit UnitFile;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnFileUtils, FMX.Edit, FMX.Memo, FMX.Types, System.Types, System.UITypes,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormFile = class(TForm)
    lblFind: TLabel;
    lblIn: TLabel;
    edtPattern: TEdit;
    edtPath: TEdit;
    btnFind: TButton;
    mmoResult: TMemo;
    btnBrowse: TButton;
    chkCancel: TCheckBox;
    dlgOpen1: TOpenDialog;
    procedure btnFindClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure OnFile(const FileName: string; const Info: TSearchRec;
      var FindAbort: Boolean);
    procedure OnDir(const SubDir: string);
  public

  end;

var
  FormFile: TFormFile;

implementation

{$R *.fmx}

procedure TFormFile.OnDir(const SubDir: string);
begin
  mmoResult.Lines.Add('! ' + SubDir);
end;

procedure TFormFile.OnFile(const FileName: string;
  const Info: TSearchRec; var FindAbort: Boolean);
begin
  Application.ProcessMessages;
  if chkCancel.IsChecked then
    FindAbort := True;
  mmoResult.Lines.Add(FileName);
end;

procedure TFormFile.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtPath.Text := ExtractFilePath(dlgOpen1.FileName);
end;

procedure TFormFile.btnFindClick(Sender: TObject);
begin
  mmoResult.Lines.Clear;
  chkCancel.IsChecked := False;

  if CnFindFile(edtPath.Text, edtPattern.Text, OnFile, OnDir) then
    ShowMessage('Find OK')
  else
    ShowMessage('Find Fail or Abort');
end;

end.
