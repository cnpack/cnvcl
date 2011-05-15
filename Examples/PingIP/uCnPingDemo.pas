{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2007 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit uCnPingDemo;
{* |<PRE>
================================================================================
* 软件名称：CnPing CnIP 测试程序
* 单元名称：CnPing CnIP 测试程序主单元
* 单元作者：胡昌洪(Sesame) sesamehch@163.com
* 备    注：
* 开发平台：PWin2000 + Delphi 5
* 兼容测试：暂无（PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6）
* 本 地 化：该窗体中的字符串暂不符合本地化处理方式
* 单元标识：$Id: uCnPingDemo.pas,v 1.1 2008/05/23 14:03:51 liuxiao Exp $
* 修改记录：2008.04.12 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Winsock, Buttons, Provider, ExtCtrls, ComCtrls, CheckLst,
  CnPing, CnIP, CnButtons, CnEdit;

type
  TfrmCnPingDemo = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnPingClick(Sender: TObject);
    procedure btnPingOnceClick(Sender: TObject);
    procedure btnPingBufferClick(Sender: TObject);
    procedure btnIPInfoClick(Sender: TObject);
  private
    { Private declarations }
    Ping: TCnPing;
    IP: TCnIp;
    FLocalIP, FResult: string;
    procedure CheckIP(Sender: TButtonControl);
  public
    { Public declarations }
  end;

var
  frmCnPingDemo: TfrmCnPingDemo;

implementation

{$R *.dfm}

procedure TfrmCnPingDemo.FormCreate(Sender: TObject);
begin
  Ping := TCnPing.Create(Self); //初始化
  IP := TCnIP.Create(Self); //初始化
  FLocalIP := IP.IPAddress;
  edtStartIP.Text := IP.IPAddress;
  edtEndIP.Text := IP.NextIP(edtStartIP.Text);
  btnIPInfoClick(btnIPInfo);
end;

procedure TfrmCnPingDemo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FreeAndNil(IP);
  FreeAndNil(Ping);
end;

procedure TfrmCnPingDemo.CheckIP(Sender: TButtonControl);
begin
  if IP.IPTypeCheck(edtStartIP.Text) = iptNone then
  begin
    ShowMessage('IP地址错误');
    Abort;
  end;
  if Sender.Tag = 0 then
  begin
    if IP.IPTypeCheck(edtEndIP.Text) = iptNone then
    begin
      ShowMessage('IP地址错误');
      Abort;
    end;
    if IP.IPToInt(edtendIP.Text) < IP.IPToInt(edtStartIP.Text) then
    begin
      ShowMessage('结束IP地址小于开始地址');
      Abort;
    end;
  end;
  pgcResult.ActivePageIndex := Sender.Tag;
end;

procedure TfrmCnPingDemo.btnPingClick(Sender: TObject);
begin
  CheckIP(TButton(Sender));
  statDemo.Panels[0].Text := '测试Ping';
  Ping.RemoteHost := IP.ComputerName;
  Ping.Ping(FResult);
  redtPing.Lines.Text := FResult;
end;

procedure TfrmCnPingDemo.btnPingOnceClick(Sender: TObject);
var
  iIP: Cardinal;
  bOnLine: Boolean;
begin
  CheckIP(TButton(Sender));
  chklstResult.Items.Clear;
  for iIP := IP.IPToInt(edtStartIP.Text) to IP.IPToInt(edtendIP.Text) do
  begin
    Ping.RemoteIP := IP.IntToIP(iIP);
    statDemo.Panels[0].Text := '正在Ping to ' + Ping.RemoteHost;
    Update;
    bOnLine := Ping.PingOnce(FResult);
    chklstResult.Items.Add(FResult);
    chklstResult.Checked[chklstResult.Items.Count - 1] := bOnLine;
    Application.ProcessMessages;
  end;
end;

procedure TfrmCnPingDemo.btnPingBufferClick(Sender: TObject);
var
  sData: string;
begin
  CheckIP(TButton(Sender));
  statDemo.Panels[0].Text := '测试PingFromBuffer';
  sData:='中国人自己的开放源码第三CnPing CnIP';
  Ping.RemoteIP := IP.IntToIP(IP.IPToInt(edtStartIP.Text));
  Ping.PingFromBuffer(sData[1], Length(sData), FResult);
  redtPingBuffer.Lines.Text := FResult;
end;

procedure TfrmCnPingDemo.btnIPInfoClick(Sender: TObject);
const
  IPINFO = '计算机名称: %0:S' + #13#10
    + '本机IP地址: %1:S' + #13#10
    + '子网掩码: %2:S' + #13#10
    + 'Mac地址: %3:S' + #13#10
    + '广播地址: %4:S' + #13#10
    + 'IP地址数: %5:D' + #13#10
    + '容纳的主机数: %6:D' + #13#10;
begin
  CheckIP(TButton(Sender));
  IP.IPAddress := FLocalIP;
  statDemo.Panels[0].Text := '本机IP信息';
  redtIPInfo.Lines.Text := Format(IPINFO, [IP.ComputerName, IP.IPAddress,
    IP.SubnetMask, IP.MacAddress, IP.BroadCastIP, IP.LocalIPCount, IP.Hosts]);
end;

end.

