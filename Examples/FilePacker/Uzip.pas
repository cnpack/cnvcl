unit Uzip;
interface
uses
    ZLIBEX,Classes,CnFilePacker;
Type
    TCnZipCompress=class(TInterfacedObject,ICnCompress)
    private
        Mi,Mo:TMemoryStream;
    public
        destructor Destroy; override;
        function GetCompressMode:TCompressMode;
        procedure DoCompressData(var AStream:TBytes;var ALength: Cardinal);
        procedure DoDeCompressData(var AStream:TBytes;var ALength: Cardinal);
    end;
implementation

destructor TCnZipCompress.Destroy;
begin
    Mi.Free;
    mi:=nil;
    Mo.Free;
    Mo:=nil;
  inherited;
end;

procedure TCnZipCompress.DoCompressData(var AStream: TBytes; var ALength:Cardinal);
begin
    if Mi= nil then
        Mi:=TMemoryStream.Create;
    if Mo= nil then
        Mo:= TMemoryStream.Create;
    mi.Clear;
    Mo.Clear;
    Mi.Write(AStream[0],ALength);
    mi.Position:=0;
    ZCompressStream(Mi,Mo);	
    mo.Position:=0;
    Mo.Read(AStream[0],Mo.Size);
    ALength:=Mo.Size;
end;

procedure TCnZipCompress.DoDeCompressData(var AStream: TBytes;var ALength:Cardinal);
begin
    if Mi= nil then
        Mi:=TMemoryStream.Create;
    if Mo= nil then
        Mo:= TMemoryStream.Create;
    mi.Clear;
    Mo.Clear;
    Mi.Write(AStream[0],ALength);
    Mi.Position:=0;
    ZDecompressStream(Mi,Mo);
    mo.Position:=0;
    SetLength(AStream,Mo.size);
    Mo.Read(AStream[0],Mo.Size);
    ALength:=Mo.Size;
end;

function TCnZipCompress.GetCompressMode: TCompressMode;
begin
    Result:=cmZIP;
end;
end.