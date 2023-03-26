program TestBloomFilter;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestBloomFilterUnit in 'TestBloomFilterUnit.pas' {BloomFilterForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBloomFilterForm, BloomFilterForm);
  Application.Run;
end.
