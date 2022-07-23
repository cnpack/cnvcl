unit UnitNTPClient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnUDP, CnNetwork;

type
  TFormNTP = class(TForm)
    lblNtp: TLabel;
    cbbNtpServers: TComboBox;
    btnSend: TButton;
    lblPort: TLabel;
    edtPort: TEdit;
    udpNTP: TCnUDP;
    procedure btnSendClick(Sender: TObject);
    procedure udpNTPDataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; FromIP: String; Port: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNTP: TFormNTP;

implementation

{$R *.DFM}

procedure TFormNTP.btnSendClick(Sender: TObject);
var
  Ntp: TCnNTPPacket;
begin
  udpNTP.RemoteHost := cbbNtpServers.Text;
  udpNTP.RemotePort := StrToIntDef(edtPort.Text, 123);

  FillChar(Ntp, SizeOf(Ntp), 0);
  Ntp.OriginateTimestamp := CnConvertDateTimeToNTPTimestamp(Now);
  CnSetNTPLeapIndicator(@Ntp, CN_NTP_LEAP_INDICATOR_NONE);
  CnSetNTPVersionNumber(@Ntp, CN_NTP_VERSION_V3);
  CnSetNTPMode(@Ntp, CN_NTP_MODE_CLIENT);
  udpNTP.SendBuffer(@Ntp, SizeOf(TCnNTPPacket));
end;

procedure TFormNTP.udpNTPDataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; FromIP: String; Port: Integer);
var
  Ntp: PCnNTPPacket;
begin
  Ntp := PCnNTPPacket(Buffer);
  ShowMessage('Get ' + IntToStr(Len) + ' Bytes from: ' + FromIP + ':' + IntToStr(Port) + #13#10#13#10 +
              'LeapIndicator: ' + IntToStr(CnGetNTPLeapIndicator(Ntp)) + #13#10 +
              'VersionNumber: ' + IntToStr(CnGetNTPVersionNumber(Ntp)) + #13#10 +
              'Mode: ' + IntToStr(CnGetNTPMode(Ntp)));
  ShowMessage('Local Origin Timestamp:' + #13#10 +
              FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
              (CnConvertNTPTimestampToDateTime(Ntp^.OriginateTimestamp))) + #13#10#13#10 +
              'GMT Transmission Timestamp:' + #13#10 +
              FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
              (CnConvertNTPTimestampToDateTime(Ntp^.TransmitTimestamp))) + #13#10#13#10 +
              'GMT Received Timestamp:' + #13#10 +
              FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
              (CnConvertNTPTimestampToDateTime(Ntp^.ReceiveTimestamp)))
              );
end;

end.
