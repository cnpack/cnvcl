unit uFrmModem;

{
  程序名称：TCnModem演示程序
  程序功能：测试TCnMode基本使用
  描述：
     1、TCnModem中的Dial函数内部完成了初始化串口、Modem等工作。

     2、用户使用TCnModem的InitAtCommand属性只可以事先定义一条AT指令。
       如果用户想加入更多的AT指令，需要修改TCnModem的源代码。
       InitModem中最好在最前面加上AT&F指令
       if not SendATOk('AT&F') then exit;
       
     3、疑问（已解决，是CnModem的bug，应该再乘以1000）：
     // 切换到在线命令状态
     procedure TCnModem.Escape; 中的下列语句让人摸不着头脑。
     Tick := Round(FWaitEscapeTime * 0.02 * 1.3);
     Sleep(Tick);
     初始化时，已经设置 S12为 FWaitEscapeTime。
     这里的Sleep(Tick);是不是有点短的出奇。而且Tick的值有经过了这么复杂的运算。

  演示程序作者：
  Written By SkyJacker
  Email:Hemiaoyu@gmail.com
  2006-12-11
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnClasses, CnRS232, CnModem, Spin, ExtCtrls;

type
  TFrmModem = class(TForm)
    cm1: TCnModem;
    grp1: TGroupBox;
    pnlLeft: TPanel;
    grp2: TGroupBox;
    edtPhone: TEdit;
    se1: TSpinEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    grp3: TGroupBox;
    btnDial: TButton;
    mmoLog: TMemo;
    lbl3: TLabel;
    edtSendData: TEdit;
    btnSend: TButton;
    btnHangUp: TButton;
    procedure btnDialClick(Sender: TObject);
    procedure cm1ReceiveData(Sender: TObject; Buffer: Pointer;
      BufferLength: Word);
    procedure btnSendClick(Sender: TObject);
    procedure btnHangUpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(const Ainfo: string);

    //初始化串口
    function  InitCom(ComNum: Word): Boolean;
    //初始化Modem
    function InitModem(): Boolean;
    //拨号
    function Dial(const PhoneNo: string):Boolean;
    //挂机
    function HangUp(): Boolean;
    //发送数据
    function SendData(const StrData: string):Boolean;
  end;

var
  FrmModem: TFrmModem;

implementation

{$R *.dfm}
  
procedure TFrmModem.Log(const Ainfo: string);
begin
  mmoLog.Lines.Add(FormatDateTime('YYYY-MM-DD HH:MM:SS',now)+ ' ' + Ainfo);
end;

function TFrmModem.InitCom(ComNum: Word): Boolean;
begin
  Result := true;
  cm1.CommName := 'COM' + Char($30+ComNum);
  Log(cm1.CommName);
end;

function TFrmModem.InitModem(): Boolean;
begin
  Result := true;
end;

function TFrmModem.HangUp(): Boolean;
begin
  Result := True;
  cm1.Hangup;
end;

function TFrmModem.Dial(const PhoneNo: string):Boolean;
var
  msgtxt: string;
begin
  Result := false;
  case cm1.Dial(PhoneNo) of
     drConnect:
       begin
         msgtxt := '连接成功';
         Result := true;
       end;
     drOpenCommFail:    msgtxt := '打开串口失败';
     drNoModem:         msgtxt := '没有检测到Modem';
     drNoDialtone:      msgtxt := '无拨号音';
     drBusy:            msgtxt := '检测到忙信号';
     drNoAnswer:        msgtxt := '无应答信号';
     drNoCarrier:       msgtxt := '没有检测到载波信号';
     drTimeout:         msgtxt := '超时错误';
     drUnknow:          msgtxt := '未知错误';
  end;
  Log(msgtxt);
end;


function TFrmModem.SendData(const StrData: string):Boolean;
begin
  result := cm1.WriteCommData(PChar(StrData),Length(StrData));
end;

procedure TFrmModem.btnDialClick(Sender: TObject);
var
  phone:string;
  iCom: Word;
begin
  iCom := se1.Value;
  InitCom(iCom);

  //收发数据
  phone := Trim(edtPhone.Text);
  if phone<>'' then
  begin
    if Dial(phone) then
    begin
      Log('开始收发数据');
    end
    else
    begin
      Log('拨号失败');
      HangUp;
    end;
  end;
end;

procedure TFrmModem.btnHangUpClick(Sender: TObject);
begin
  HangUp;
end;

procedure TFrmModem.cm1ReceiveData(Sender: TObject; Buffer: Pointer;
  BufferLength: Word);
var
   I: Integer;
  S: string;
  Input: array of byte;
begin
  Input := Buffer;
  for I:=0 to BufferLength-1 do
  begin
    S := S + Chr(Input[I]);
  end;
  Log('收到：'+S);
end;

procedure TFrmModem.btnSendClick(Sender: TObject);
var
  sData: string;
begin
  sData := edtSendData.Text;
  if SendData(sData) then
    Log('发送成功: ' + sData)
  else
    Log('发送失败' + sData);
end;

end.
