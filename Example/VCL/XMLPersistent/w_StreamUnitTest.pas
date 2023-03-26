unit w_StreamUnitTest;

interface

uses classes, ExtCtrls;

type
  TStreamOwner = class(TPersistent)
  private
    FImg: TImage;
    FMem: TMemoryStream;
    function GetStream: TStream;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Img: TImage read FImg;
    property Stream: TStream read GetStream;
  end;

implementation

uses SysUtils;

constructor TStreamOwner.Create;
begin
  inherited;
  FMem := TMemoryStream.Create();
  FImg := TImage.Create(nil);
end;

destructor TStreamOwner.Destroy;
begin
  FreeAndNil(FImg);
  FreeAndNil(FMem);
  inherited Destroy;
end;

function TStreamOwner.GetStream: TStream;
begin
  Result := FMem;
end;

end.
