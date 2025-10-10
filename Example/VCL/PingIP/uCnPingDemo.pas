{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2007 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit uCnPingDemo;
{* |<PRE>
================================================================================
* ������ƣ�CnPing CnIP ���Գ���
* ��Ԫ���ƣ�CnPing CnIP ���Գ�������Ԫ
* ��Ԫ���ߣ�������(Sesame) sesamehch@163.com
* ��    ע��
* ����ƽ̨��PWin2000 + Delphi 5
* ���ݲ��ԣ����ޣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6��
* �� �� �����ô����е��ַ����ݲ����ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: uCnPingDemo.pas,v 1.1 2008/05/23 14:03:51 liuxiao Exp $
* �޸ļ�¼��2008.04.12 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Winsock, Buttons, Provider, ExtCtrls, ComCtrls, CheckLst,
  CnPing, CnIP, CnButtons, CnEdit, CnNative, CnInt128;

type
  TFormPingDemo = class(TForm)
    Label1: TLabel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    edtStartIP: TCnEdit;
    edtEndIP: TCnEdit;
    Panel3: TPanel;
    statDemo: TStatusBar;
    pgcResult: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    btnPingOnce: TCnBitBtn;
    btnPingBuffer: TCnBitBtn;
    btnIPInfo: TCnBitBtn;
    btnPing: TCnBitBtn;
    chklstResult: TCheckListBox;
    redtIPInfo: TRichEdit;
    redtPingBuffer: TRichEdit;
    redtPing: TRichEdit;
    grpIPv6: TGroupBox;
    btnCalcv6: TCnBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnPingClick(Sender: TObject);
    procedure btnPingOnceClick(Sender: TObject);
    procedure btnPingBufferClick(Sender: TObject);
    procedure btnIPInfoClick(Sender: TObject);
    procedure btnCalcv6Click(Sender: TObject);
  private
    Ping: TCnPing;
    IP: TCnIp;
    FLocalIP, FResult: string;
    procedure CheckIP(Sender: TButtonControl);
  public

  end;

var
  FormPingDemo: TFormPingDemo;

implementation

{$R *.dfm}

procedure TFormPingDemo.FormCreate(Sender: TObject);
begin
  Ping := TCnPing.Create(Self); //��ʼ��
  IP := TCnIP.Create(Self); //��ʼ��
  FLocalIP := IP.IPAddress;
  edtStartIP.Text := IP.IPAddress;
  edtEndIP.Text := IP.NextIP(edtStartIP.Text);
  btnIPInfoClick(btnIPInfo);
end;

procedure TFormPingDemo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FreeAndNil(IP);
  FreeAndNil(Ping);
end;

procedure TFormPingDemo.CheckIP(Sender: TButtonControl);
begin
  if IP.IPTypeCheck(edtStartIP.Text) = iptNone then
  begin
    ShowMessage('IP��ַ����');
    Abort;
  end;
  if Sender.Tag = 0 then
  begin
    if IP.IPTypeCheck(edtEndIP.Text) = iptNone then
    begin
      ShowMessage('IP��ַ����');
      Abort;
    end;
    if IP.IPToInt(edtendIP.Text) < IP.IPToInt(edtStartIP.Text) then
    begin
      ShowMessage('����IP��ַС�ڿ�ʼ��ַ');
      Abort;
    end;
  end;
  pgcResult.ActivePageIndex := Sender.Tag;
end;

procedure TFormPingDemo.btnPingClick(Sender: TObject);
begin
  CheckIP(TButton(Sender));
  statDemo.Panels[0].Text := '����Ping';
  Ping.RemoteHost := IP.ComputerName;
  Ping.Ping(FResult);
  redtPing.Lines.Text := FResult;
end;

procedure TFormPingDemo.btnPingOnceClick(Sender: TObject);
var
  iIP: Cardinal;
  bOnLine: Boolean;
begin
  CheckIP(TButton(Sender));
  chklstResult.Items.Clear;
  for iIP := IP.IPToInt(edtStartIP.Text) to IP.IPToInt(edtendIP.Text) do
  begin
    Ping.RemoteIP := IP.IntToIP(iIP);
    statDemo.Panels[0].Text := '����Ping to ' + Ping.RemoteHost;
    Update;
    bOnLine := Ping.PingOnce(FResult);
    chklstResult.Items.Add(FResult);
    chklstResult.Checked[chklstResult.Items.Count - 1] := bOnLine;
    Application.ProcessMessages;
  end;
end;

procedure TFormPingDemo.btnPingBufferClick(Sender: TObject);
var
  sData: string;
begin
  CheckIP(TButton(Sender));
  statDemo.Panels[0].Text := '����PingFromBuffer';
  sData:='�й����Լ��Ŀ���Դ�������������CnPing CnIP';
  Ping.RemoteIP := IP.IntToIP(IP.IPToInt(edtStartIP.Text));
  Ping.PingFromBuffer(sData[1], Length(sData) * SizeOf(Char), FResult);
  redtPingBuffer.Lines.Text := FResult;
end;

procedure TFormPingDemo.btnIPInfoClick(Sender: TObject);
const
  IPINFO = '���������: %s' + #13#10
    + '����IPv4��ַ: %s' + #13#10
    + 'IPv4��������: %s' + #13#10
    + '����IPv6��ַ: %s' + #13#10
    + 'IPv6ǰ׺����: %d' + #13#10
    + 'IPv6��������: %s' + #13#10
    + 'Mac��ַ: %s' + #13#10
    + '�㲥��ַ: %s' + #13#10
    + 'IPv4��ַ��: %d' + #13#10
    + '���ɵ�IPv4������: %d' + #13#10;
  BOOL_STRS: array[False..True] of string = ('False', 'True');
var
  I: Integer;
  IpGroups: TCnIPGroup;
  Ip6Groups: TCnIPv6Group;
begin
  CheckIP(TButton(Sender));
  IP.IPAddress := FLocalIP;
  statDemo.Panels[0].Text := '����IP��Ϣ';
  redtIPInfo.Lines.Text := Format(IPINFO, [IP.ComputerName, IP.IPAddress, IP.SubnetMask,
    IP.IPv6Address, IP.IPv6PrefixLength, IP.IPv6SubnetMask, IP.MacAddress,
    IP.BroadCastIP, IP.LocalIPCount, IP.Hosts]);

  IpGroups := IP.LocalIPGroup;
  for I := Low(IpGroups) to High(IpGroups) do
  begin
    redtIPInfo.Lines.Add('================ ' + IntToStr(I));
    redtIPInfo.Lines.Add(IP.IntToIP(IpGroups[I].IPAddress));
    redtIPInfo.Lines.Add(IP.IntToIP(IpGroups[I].SubnetMask));
    redtIPInfo.Lines.Add(IP.IntToIP(IpGroups[I].BroadCast));
    redtIPInfo.Lines.Add('UpState ' + BOOL_STRS[IpGroups[I].UpState]);
    redtIPInfo.Lines.Add('Loopback ' + BOOL_STRS[IpGroups[I].Loopback]);
    redtIPInfo.Lines.Add('SupportBroadcast ' + BOOL_STRS[IpGroups[I].SupportBroadcast]);
  end;

  redtIPInfo.Lines.Add('IPv6��ַ��: ' + IntToStr(IP.LocalIPv6Count));
  Ip6Groups := IP.LocalIPv6Group;
  for I := Low(Ip6Groups) to High(Ip6Groups) do
  begin
    redtIPInfo.Lines.Add('================ ' + IntToStr(I));
    redtIPInfo.Lines.Add(IP.Int128ToIPv6(Ip6Groups[I].IPv6Address));
    redtIPInfo.Lines.Add(IntToStr(Ip6Groups[I].PrefixLength));
    redtIPInfo.Lines.Add(DataToHex(@Ip6Groups[I].SubnetMask[0], SizeOf(TCnIPv6NetMask)));
    redtIPInfo.Lines.Add(IP.Int128ToIPv6(Ip6Groups[I].BroadCast));
    redtIPInfo.Lines.Add('UpState ' + BOOL_STRS[Ip6Groups[I].UpState]);
    redtIPInfo.Lines.Add('Loopback ' + BOOL_STRS[Ip6Groups[I].Loopback]);
    redtIPInfo.Lines.Add('SupportBroadcast ' + BOOL_STRS[Ip6Groups[I].SupportBroadcast]);
  end;
end;

procedure TFormPingDemo.btnCalcv6Click(Sender: TObject);
const
  IPv61 = 'fd00:c2b6:b24b:be67:2827:688d:e6a1:6a3b';
  IPv62 = '1::/64';
  IPv63 = '::1234:5678/64';
  IPv64 = '2001:0db8::8a2e:0370:7334/64';
var
  R: TCnUInt128;
begin
  redtIPInfo.Lines.Clear;

  R := TCnIp.IPv6ToInt128(IPv61);
  redtIPInfo.Lines.Add(TCnIp.Int128ToIPv6(R));
  R := TCnIp.IPv6ToInt128(IPv62);
  redtIPInfo.Lines.Add(TCnIp.Int128ToIPv6(R));
  R := TCnIp.IPv6ToInt128(IPv63);
  redtIPInfo.Lines.Add(TCnIp.Int128ToIPv6(R));
  R := TCnIp.IPv6ToInt128(IPv64);
  redtIPInfo.Lines.Add(TCnIp.Int128ToIPv6(R));
end;

end.

