unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnThreadPool, IdTCPServer, IdBaseComponent,
  IdComponent, IdTCPClient, Buttons;

type
  TfrmTest = class(TForm)
    btn1: TButton;
    edt1: TEdit;
    lbl1: TLabel;
    mmo1: TMemo;
    btn2: TButton;
    edt2: TComboBox;
    lbl2: TLabel;
    rg1: TRadioGroup;
    edt3: TEdit;
    mmo2: TMemo;
    IdTCPServer1: TIdTCPServer;
    lbl3: TLabel;
    edt4: TEdit;
    tmr1: TTimer;
    btn3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure edt1Change(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
  private
    { Private declarations }
    PoolSend: TCnThreadPool;
    RecivedCount: Integer;
    SendCount, ProcessCount: Integer;
    Sending: Boolean;
    iSendInterval: Integer;
    csReciver: TCnCriticalSection;
    sReadLn: string;

    procedure SeparateHostAndPort(const s: string; var host: string;
      var port: Integer);
    procedure Updatemmo1;
    procedure ProcessRequest(Sender: TCnThreadPool;
      aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
    public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

uses StrUtils, IdSocketHandle;

{$R *.DFM}

procedure Delay(const i: DWORD);
var
  t: DWORD;
begin
  t := GetTickCount;
  while (not Application.Terminated) and (GetTickCount - t < i) do
    Application.ProcessMessages
end;

type
  TSendData = class(TCnTaskDataObject)
  private
    FHost: string;
    FPort: Integer;
    FMsg: string;
    FCanMerge: Boolean;
  public
    constructor Create(const ahost: string; aport: Integer; amsg: string;
      amerge: Boolean);

    function Clone: TCnTaskDataObject; override;  
    function Duplicate(DataObj: TCnTaskDataObject;
      const Processing: Boolean): Boolean; override;
    function Info: string; override;
  end;

  TSendThread = class(TCnPoolingThread)
  private
    FIdTCPClient: TIdTCPClient;
  public
    constructor Create(aPool: TCnThreadPool); override;
    destructor Destroy; override;
  end;

{ TSendData }

function TSendData.Clone: TCnTaskDataObject;
begin
  Result := TSendData.Create(FHost, FPort, FMsg, FCanMerge);
end;

constructor TSendData.Create(const ahost: string; aport: Integer;
  amsg: string; amerge: Boolean);
begin
  FHost := ahost;
  FPort := aport;
  FMsg := amsg;
  FCanMerge := amerge;
end;

function TSendData.Duplicate(DataObj: TCnTaskDataObject;
  const Processing: Boolean): Boolean;
begin
  Result := (not Processing) and
    FCanMerge and TSendData(DataObj).FCanMerge and
    (FHost = TSendData(DataObj).FHost) and
    (FPort = TSendData(DataObj).FPort);
  if Result then
    TSendData(DataObj).FMsg := TSendData(DataObj).FMsg + '#' + FMsg
end;

function TSendData.Info: string;
begin
  Result := 'IP=' + FHost + ':' + IntToStr(FPort) + ';Len(Msg)=' + IntToStr(Length(FMsg));
  if FCanMerge then
    Result := Result + ';Can Merge'
end;

{ TSendThread }

constructor TSendThread.Create(aPool: TCnThreadPool);
begin
  //OutputDebugString('TSendThread.Create');
  inherited;
  FIdTCPClient := TIdTCPClient.Create(nil)
end;

destructor TSendThread.Destroy;
begin
  FIdTCPClient.Disconnect;
  FIdTCPClient.Free;
  inherited;
  //OutputDebugString('TSendThread.Destroy');
end;

{ TfrmTest }

procedure TfrmTest.SeparateHostAndPort(const s: string;
  var host: string; var port: Integer);
var
  i: Integer;
begin
  i := Pos(':', s);
  if i > 0 then
  begin
    host := Copy(s, 1, i - 1);
    port := StrToIntDef(Copy(s, i + 1, MaxInt), IdTCPServer1.DefaultPort)
  end
  else
  begin
    host := s;
    port := IdTCPServer1.DefaultPort
  end
end;

procedure TfrmTest.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TMemo then
      TMemo(Controls[i]).Clear;
  csReciver := TCnCriticalSection.Create;
  PoolSend := TCnThreadPool.CreateSpecial(Self, TSendThread);
  with PoolSend do
  begin
    OnProcessRequest := ProcessRequest;
    AdjustInterval := 5 * 1000;
    MinAtLeast := False;
    ThreadDeadTimeout := 10 * 1000;
    ThreadsMinCount := 0;
    ThreadsMaxCount := 50;
    uTerminateWaitTime := 2 * 1000;
  end;
  RecivedCount := 0;
  SendCount := 0;
  ProcessCount := 0;
  IdTCPServer1.DefaultPort := StrToIntDef(edt1.Text, 5999);
  iSendInterval := StrToIntDef(edt4.Text, 5999)
end;

procedure TfrmTest.IdTCPServer1Execute(AThread: TIdPeerThread);
begin
  sReadLn := AThread.Connection.ReadLn();
  AThread.Synchronize(Updatemmo1);
end;

procedure TfrmTest.edt1Change(Sender: TObject);
var
  i, port, newport: Integer;
  host, s: string;
begin
  if not IdTCPServer1.Active then
  begin
    newport := StrToIntDef(edt1.Text, IdTCPServer1.DefaultPort);
    IdTCPServer1.DefaultPort := newport;
    edt1.Text := IntToStr(newport);
    SeparateHostAndPort(edt2.Text, host, port);
    s := host + ':' + IntToStr(newport);
    for i := 0 to edt2.Items.Count - 1 do
    begin
      SeparateHostAndPort(edt2.Items.Strings[i], host, port);
      edt2.Items.Strings[i] := host + ':' + IntToStr(newport)
    end;
    edt2.Text := s
  end
  else
    edt1.Text := IntToStr(IdTCPServer1.DefaultPort)
end;

procedure TfrmTest.btn2Click(Sender: TObject);
var
  i: Integer;
  host: string;
  port: Integer;
begin
  Sending := not Sending;
  btn2.Caption := IfThen(Sending, '停止发送', '开始发送' + IntToStr(SendCount));
  if Sending then
  begin
    iSendInterval := StrToIntDef(edt4.Text, iSendInterval);
    SendCount := 0;
    ProcessCount := 0;
    while Sending and not Application.Terminated do
    begin
      SeparateHostAndPort(edt2.Text, host, port);
      PoolSend.AddRequest(TSendData.Create(host, port, edt3.Text, rg1.ItemIndex = 0), [cdQueue]);
      Inc(SendCount);
      btn2.Caption := '停止发送' + IntToStr(SendCount);
      case iSendInterval of
        0..5: begin
          Application.ProcessMessages
        end;
        6..10: begin
          Sleep(iSendInterval);
          Application.ProcessMessages
        end;
        11..50: begin
          Sleep(iSendInterval div 2);
          Delay(iSendInterval div 2)
        end;
      else
        for i := 0 to iSendInterval div 50 - 1 do
        begin
          Sleep(10);
          Delay(40)
        end
      end;
    end
  end
end;

procedure TfrmTest.btn1Click(Sender: TObject);
begin
  RecivedCount := 0;
  if not IdTCPServer1.Active then
  begin
    IdTCPServer1.Bindings.Clear;
    with IdTCPServer1.Bindings.Add do
    begin
      IP := '0.0.0.0';//'127.0.0.1';
      Port := IdTCPServer1.DefaultPort
    end
  end;
  IdTCPServer1.Active := not IdTCPServer1.Active;
  btn1.Caption := IfThen(IdTCPServer1.Active,
      '停止监听' + IntToStr(IdTCPServer1.DefaultPort),
      '开始监听')
end;

procedure TfrmTest.tmr1Timer(Sender: TObject);
begin
  mmo2.Text := PoolSend.Info
end;

procedure TfrmTest.ProcessRequest(Sender: TCnThreadPool;
  aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
var
  d: TSendData;
  t: TSendThread;
begin
  d := TSendData(aDataObj);
  t := TSendThread(aThread);
  if (d = nil) or (t = nil) then
    Exit;

  Inc(ProcessCount);
  //OutputDebugString(PChar('ProcessRequest: ' + IntToStr(ProcessCount)));
  t.FIdTCPClient.Host := d.FHost;
  t.FIdTCPClient.Port := d.FPort;
  if t.FIdTCPClient.Connected then
    t.FIdTCPClient.Disconnect;
  try
    try
      t.FIdTCPClient.Connect();
      if d.FMsg <> '' then
        t.FIdTCPClient.WriteLn(d.FMsg)
      else
        Sleep(1000);
    finally
      if t.FIdTCPClient.Connected then
        t.FIdTCPClient.Disconnect
    end;
  except
  end;
  //Sleep(10)
end;

procedure TfrmTest.FormDestroy(Sender: TObject);
begin
  PoolSend.Free;
  csReciver.Free;
end;

procedure TfrmTest.Updatemmo1;
begin
  Inc(RecivedCount);
  mmo1.Lines.Add(IntToStr(RecivedCount) + ':' + sReadln)
end;

procedure TfrmTest.btn3Click(Sender: TObject);
begin
  Application.MessageBox(
    '====说明====' + #13#10 +
    '  本线程池在Windows平台中工作，在Win9x平台中不能动态减少线程数，在NT架构平台中能够有最佳的表现。' + #13#10 + #13#10 +
    '====界面====' + #13#10 + 
    '  “开始监听”按钮会监听当前计算机的某个端口，它绑定了该计算机的所有IP，如127.0.0.1等，如果需要接收线程池发送的TCP信息，就可以在目标计算机上点击该按钮，再次点击该按钮取消监听。' + #13#10 +
    '  “开始监听”下面的文本框会显示收到的TCP信息。由于Indy的TIdTCPServer的实现机制限制，当发送者发送的频率很大时，可能会导致监听服务器产生很多的线程，而为了能够按次序正确的显示收到的信息，程序使用了同步。' + '如果等待线程太多时，可能会导致TIdTCPServer不能继续工作。' + #13#10 +
    '  “开始发送”将会每隔一定时间不断的向目标地址发送TCP信息，再次点击停止发送。' + #13#10 +
    '  “发送选项-可拼合”表示新的TCP信息可以与队列中的信息进行拼合处理，经过该预处理之后线程池将会有更好的延展性。' + #13#10 +
    '  “发送选项-不可拼合”表示新的TCP信息与队列中信息无法拼合。' + #13#10 +
    '  “发送选项”下的文本框为待发送的内容，如果启用了发送拼合，则被拼合的TCP信息的发送内容通过#分隔。' + #13#10 +
    '  最下面的文本框每隔一秒刷新一次线程池的运行状态。' + #13#10 + #13#10 +
    '====测试====' + #13#10 + 
    '  网络通讯有很多种情形，典型的有：通畅，IP存在无监听，IP不存在。这三种情形所需要的工作时间也大不同，通畅的情形下可能只要几毫秒就可以完成一次通信，而IP存在无监听的情形一般需要几百毫秒到几秒之间，IP不存在可能需要几十秒。' + '这是比较复杂的实际情形的模拟，应该可以代表实际的应用。' + #13#10 +
    '  通畅的测试，在目标计算机上点击“开始监听”（可以使用同一计算机），发送计算机“开始发送”，当发送间隔较小时如果启用了可拼合选项一般也只需要一个工作线程，而即使不可拼合也不会有很多的线程（为了增加工作时间，工作线程有Sleep(10)）。' + '在不可拼合下达到平衡时切换到“可拼合”应该也不会减少工作线程，因为实际上不可拼合的线程数才是真正需要的线程数。' + #13#10 +
    '  IP存在无监听，目标计算机“停止监听”即可。这时候不可拼合的工作线程数应该比可拼合增加一些，当从该状态转变到通畅状态时，线程池应该可以动态的减低线程数。' + #13#10 +
    '  IP不存在，极耗工作时间，不可拼合时应该会有很多的线程。不过，即使在该状态下直接退出程序也不应该会造成程序的异常（不过由于这时候为了能够及时退出程序，会强行中止了工作线程）。'
    , '线程池测试方法', 0);
end;

procedure TfrmTest.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  shift: TShiftState;
begin
  if ActiveControl is TCustomEdit then
  begin
    shift := KeyDataToShiftState(Msg.KeyData);
    if (ssCtrl in shift) and (Msg.CharCode = Ord('A')) then
    begin
      TCustomEdit(ActiveControl).SelectAll;
      Handled := True
    end
  end
  else if ActiveControl is TCustomCombo then
  begin
    shift := KeyDataToShiftState(Msg.KeyData);
    if (ssCtrl in shift) and (Msg.CharCode = Ord('A')) then
    begin
      TCustomCombo(ActiveControl).SelectAll;
      Handled := True
    end
  end
end;

end.
