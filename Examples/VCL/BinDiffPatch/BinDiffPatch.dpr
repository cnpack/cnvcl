program BinDiffPatch;

uses
  Forms,
  BinDiffPatchUnit in 'BinDiffPatchUnit.pas' {BinaryDiffPatchForm},
  CnBinaryDiffPatch in '..\..\..\Source\Common\CnBinaryDiffPatch.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBinaryDiffPatchForm, BinaryDiffPatchForm);
  Application.Run;
end.
