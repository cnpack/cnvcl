unit UnitFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnFileUtils;

type
  TFormFile = class(TForm)
    lblFind: TLabel;
    edtPattern: TEdit;
    lblIn: TLabel;
    edtPath: TEdit;
    btnFind: TButton;
    mmoResult: TMemo;
    btnBrowse: TButton;
    dlgOpen1: TOpenDialog;
    chkCancel: TCheckBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
  private
    procedure OnFile(const FileName: string; const Info: TSearchRec;
      var FindAbort: Boolean);
    procedure OnDir(const SubDir: string);
  public

  end;

var
  FormFile: TFormFile;

implementation

{$R *.DFM}

procedure TFormFile.OnDir(const SubDir: string);
begin
  mmoResult.Lines.Add('! ' + SubDir);
end;

procedure TFormFile.OnFile(const FileName: string;
  const Info: TSearchRec; var FindAbort: Boolean);
begin
  Application.ProcessMessages;
  if chkCancel.Checked then
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
  chkCancel.Checked := False;

  if CnFindFile(edtPath.Text, edtPattern.Text, OnFile, OnDir) then
    ShowMessage('Find OK')
  else
    ShowMessage('Find Fail or Abort');
end;

end.
