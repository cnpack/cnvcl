unit UnitDNS;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnUDP, CnNetwork, CnNative, CnDNS, CnClasses;

type
  TFormDNS = class(TForm)
    lblNameServer: TLabel;
    edtDNSServer: TEdit;
    lblHostName: TLabel;
    edtHostName: TEdit;
    btnQuery: TButton;
    udpDNS: TCnUDP;
    mmoResponse: TMemo;
    btnTestParseString: TButton;
    btnDNS: TButton;
    CnDNS1: TCnDNS;
    btnDNSDesign: TButton;
    procedure btnQueryClick(Sender: TObject);
    procedure udpDNSDataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; const FromIP: String; Port: Integer);
    procedure btnTestParseStringClick(Sender: TObject);
    procedure btnDNSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CnDNS1Response(Sender: TObject;
      Response: TCnDNSPacketObject);
    procedure btnDNSDesignClick(Sender: TObject);
  private
    FDNS: TCnDNS;
    procedure DNSResponse(Sender: TObject; Response: TCnDNSPacketObject);
  public
    { Public declarations }
  end;

var
  FormDNS: TFormDNS;

implementation

{$R *.LFM}

const
  SAMPLE_RESPONSE: array[0..371] of Byte = (
    $25, $37, $81, $80, $00, $01, $00, $02, $00, $02, $00, $10, $03, $77, $77, $77,
    $06, $63, $6E, $70, $61, $63, $6B, $03, $6F, $72, $67, $00, $00, $01, $00, $01,
    $C0, $0C, $00, $05, $00, $01, $00, $00, $01, $2C, $00, $02, $C0, $10, $C0, $10,
    $00, $01, $00, $01, $00, $00, $02, $58, $00, $04, $68, $EE, $8C, $BC, $C0, $10,
    $00, $02, $00, $01, $00, $01, $51, $80, $00, $14, $07{以此为入口，偏移量$4A}, $66, $31, $67, $31, $6E,
    $73, $31, $06, $64, $6E, $73, $70, $6F, $64, $03, $6E, $65, $74, $00, $C0, $10,
    $00, $02, $00, $01, $00, $01, $51, $80, $00, $0A, $07{以此为入口，偏移量$6A, MaxLen 10}, $66, $31, $67, $31, $6E,
    $73, $32, $C0, $52, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $65, $E2, $DC, $10, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $6F, $A1, $39, $51, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $B6, $8C, $A7, $BC, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $B7, $C0, $C9, $5A, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $0E, $D7, $9B, $CB, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $3A, $F7, $D4, $30, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $3A, $F7, $D4, $77, $C0, $6A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $3D, $81, $08, $9F, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $B6, $8C, $A7, $A6, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $0E, $D7, $96, $11, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $0E, $D7, $9B, $9C, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $0E, $D7, $9B, $AA, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $3A, $F7, $D4, $24, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $3D, $97, $B4, $2C, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $6F, $A1, $39, $4D, $C0, $4A, $00, $01, $00, $01, $00, $00, $00, $80, $00, $04,
    $B4, $A3, $13, $0F
  );

procedure TFormDNS.btnQueryClick(Sender: TObject);
var
  Buf: TBytes;
begin
  mmoResponse.Lines.Clear;
  udpDNS.RemoteHost := edtDNSServer.Text;
  udpDNS.RemotePort := 53;
  Buf := TCnDNS.BuildDNSQueryPacket(edtHostName.Text, 9527);
  udpDNS.SendBuffer(@Buf[0], Length(Buf));
end;

procedure TFormDNS.udpDNSDataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; const FromIP: String; Port: Integer);
var
  Packet: TCnDNSPacketObject;
  P: PCnDNSHeader;
begin
  mmoResponse.Lines.Add(Format('Get %d Bytes from %s:%d', [Len, FromIP, Port]));
  P := PCnDNSHeader(Buffer);
  mmoResponse.Lines.Add(Format('DNS Packet Id %d.', [P^.Id]));
  Packet := TCnDNSPacketObject.Create;
  TCnDNS.ParseDNSResponsePacket(PAnsiChar(Buffer), Len, Packet);
  Packet.DumpToStrings(mmoResponse.Lines);
  Packet.Free;
end;

procedure TFormDNS.btnTestParseStringClick(Sender: TObject);
var
  S: string;
  R: Integer;
begin
  S := '';
  R := TCnDNS.ParseIndexedString(S, @SAMPLE_RESPONSE[0], @SAMPLE_RESPONSE[$4A]); // , SizeOf(SAMPLE_RESPONSE));
  ShowMessage(S + '    Step: ' + IntToStr(R));
  S := '';
  R := TCnDNS.ParseIndexedString(S, @SAMPLE_RESPONSE[0], @SAMPLE_RESPONSE[$6A], 10); // , SizeOf(SAMPLE_RESPONSE));
  ShowMessage(S + '    Step: ' + IntToStr(R));
end;

procedure TFormDNS.btnDNSClick(Sender: TObject);
begin
  FDNS.NameServerIP := edtDNSServer.Text;
  FDNS.SendHostQuery(edtHostName.Text);
end;

procedure TFormDNS.DNSResponse(Sender: TObject;
  Response: TCnDNSPacketObject);
begin
  Response.DumpToStrings(mmoResponse.Lines);
end;

procedure TFormDNS.FormCreate(Sender: TObject);
begin
  FDNS := TCnDNS.Create(Self);
  FDNS.OnResponse := DNSResponse;
end;

procedure TFormDNS.CnDNS1Response(Sender: TObject;
  Response: TCnDNSPacketObject);
begin
  Response.DumpToStrings(mmoResponse.Lines);
end;

procedure TFormDNS.btnDNSDesignClick(Sender: TObject);
begin
  CnDNS1.NameServerIP := edtDNSServer.Text;
  CnDNS1.SendHostQuery(edtHostName.Text);
end;

end.
