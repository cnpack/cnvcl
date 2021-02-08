program TestBloomFilter;

uses
  Forms,
  TestBloomFilterUnit in 'TestBloomFilterUnit.pas' {BloomFilterForm},
  CnBloomFilter in '..\..\Source\Common\CnBloomFilter.pas',
  CnCRC32 in '..\..\Source\Crypto\CnCRC32.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBloomFilterForm, BloomFilterForm);
  Application.Run;
end.
