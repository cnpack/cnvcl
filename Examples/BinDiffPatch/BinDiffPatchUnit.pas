unit BinDiffPatchUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TBinaryDiffPatchForm = class(TForm)
    grpDiff: TGroupBox;
    lblDiffOld: TLabel;
    lblDiffNew: TLabel;
    edtDiffOld: TEdit;
    edtDiffNew: TEdit;
    btnDiffBrowseOld: TButton;
    btnDiffBrowseNew: TButton;
    btnDiff: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure btnDiffBrowseOldClick(Sender: TObject);
    procedure btnDiffBrowseNewClick(Sender: TObject);
    procedure btnDiffClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BinaryDiffPatchForm: TBinaryDiffPatchForm;

implementation

uses CnBinaryDiffPatch;

{$R *.DFM}

procedure TBinaryDiffPatchForm.btnDiffBrowseOldClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDiffOld.Text := dlgOpen.FileName;
end;

procedure TBinaryDiffPatchForm.btnDiffBrowseNewClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDiffNew.Text := dlgOpen.FileName;
end;

procedure TBinaryDiffPatchForm.btnDiffClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    if CnBinaryDiffPatch.BinaryDiffFile(edtDiffOld.Text, edtDiffNew.Text, dlgSave.FileName) then
      ShowMessage('Diff File Save to ' + dlgSave.FileName)
    else
      ShowMessage('Diff File Fail !');
  end;
end;

end.
