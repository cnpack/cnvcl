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
    edtPatch: TEdit;
    lblPatch: TLabel;
    btnDiffBrowsePatch: TButton;
    btnBinaryPatch: TButton;
    edtPatchNew: TEdit;
    lblPatchNew: TLabel;
    btnPatchBrowseNew: TButton;
    procedure btnDiffBrowseOldClick(Sender: TObject);
    procedure btnDiffBrowseNewClick(Sender: TObject);
    procedure btnDiffClick(Sender: TObject);
    procedure btnDiffBrowsePatchClick(Sender: TObject);
    procedure btnBinaryPatchClick(Sender: TObject);
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
  if Trim(edtPatch.Text) <> '' then
  begin
    if CnBinaryDiffPatch.BinaryDiffFile(edtDiffOld.Text, edtDiffNew.Text, Trim(edtPatch.Text)) then
      ShowMessage('Diff File Save to ' + Trim(edtPatch.Text))
    else
      ShowMessage('Diff File Fail !');
  end;
end;

procedure TBinaryDiffPatchForm.btnDiffBrowsePatchClick(Sender: TObject);
begin
  if dlgSave.Execute then
    edtPatch.Text := dlgSave.FileName;
end;

procedure TBinaryDiffPatchForm.btnBinaryPatchClick(Sender: TObject);
begin
  if Trim(edtPatch.Text) <> '' then
  begin
    if CnBinaryDiffPatch.BinaryPatchFile(edtDiffOld.Text, Trim(edtPatch.Text), edtPatchNew.Text) then
      ShowMessage('Restore File From Patch Save to ' + edtPatchNew.Text)
    else
      ShowMessage('Restore File From Patch Fail !');
  end;
end;

end.
