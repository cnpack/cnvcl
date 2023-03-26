program BinDiffPatch;

uses
  System.StartUpCopy,
  FMX.Forms,
  BinDiffPatchUnit in 'BinDiffPatchUnit.pas' {BinaryDiffPatchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBinaryDiffPatchForm, BinaryDiffPatchForm);
  Application.Run;
end.
