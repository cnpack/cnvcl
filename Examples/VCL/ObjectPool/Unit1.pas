unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnADOConPool, CnObjectPool, CnThreadPool, StdCtrls,
  ComCtrls, ExtCtrls, Spin, SyncObjs, Buttons, CnClasses;

type
  TForm1 = class(TForm)
    CnThreadPool1: TCnThreadPool;
    CnObjectPool1: TCnObjectPool;
    CnADOConPool1: TCnADOConPool;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo2: TMemo;
    Memo3: TMemo;
    Timer1: TTimer;
    CnThreadPool2: TCnThreadPool;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo4: TMemo;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Button1: TButton;
    SpinEdit4: TSpinEdit;
    CheckBox1: TCheckBox;
    SpinEdit6: TSpinEdit;
    Panel2: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    SpinEdit5: TSpinEdit;
    SpinEdit7: TSpinEdit;
    SpinEdit8: TSpinEdit;
    Button2: TButton;
    SpinEdit9: TSpinEdit;
    CheckBox2: TCheckBox;
    SpinEdit10: TSpinEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    Button3: TButton;
    ComboBox7: TComboBox;
    Label11: TLabel;
    Label12: TLabel;
    ComboBox8: TComboBox;
    Memo5: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    CheckBox3: TCheckBox;
    Label13: TLabel;
    SpinEdit11: TSpinEdit;
    Label14: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure CnThreadPool2ProcessRequest(Sender: TCnThreadPool;
      aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
    procedure CnThreadPool1ProcessRequest(Sender: TCnThreadPool;
      aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure SpinEdit7Change(Sender: TObject);
    procedure SpinEdit8Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CnADOConPool1ReInitOne(Pool: TCnCustomObjectPool;
      Wrapper: TCnObjectWrapper; var Obj: TObject; var bSuccess: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpinEdit11Change(Sender: TObject);
  private
    { Private declarations }
    //NeedSelectItem: Boolean;
    cs: TCriticalSection;
    sslog: TStringList;
    iInTimer: Integer;
    procedure Output(const s: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DB, ADODB, ActiveX, Math, Unit2;

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  if (iInTimer > 0) or Application.Terminated then
    Exit;

  InterlockedIncrement(iInTimer);
  try
    if CheckBox1.Checked then
      for i := 0 to SpinEdit6.Value - 1 do
        CnThreadPool1.AddRequest(TCnTaskDataObject.Create, []);
    if CheckBox2.Checked then
      for i := 0 to SpinEdit10.Value - 1 do
        CnThreadPool2.AddRequest(TCnTaskDataObject.Create, []);

    //{
    Memo1.Text := CnThreadPool1.Info;
    Memo2.Text := CnObjectPool1.GetInfo;
    Memo3.Text := CnADOConPool1.GetInfo;
    Memo4.Text := CnThreadPool2.Info;
    {}

    {
    if NeedSelectItem then
    begin
      Memo5.ItemIndex := Memo5.Count - 1;
      NeedSelectItem := False;
    end;
    }
    if CheckBox3.Checked then
    begin
      cs.Enter;
      try
        if sslog.Count > 0 then
        begin
          while sslog.Count > 0 do
          begin
            Memo5.Items.Add(sslog.Strings[0]);
            sslog.Delete(0);
          end;
          Memo5.ItemIndex := Memo5.Items.Count - 1;
        end;
      finally
        cs.Leave;
      end;
    end;
  finally
    InterlockedDecrement(iInTimer);
  end;
end;

function grToStr(const gr: TCnObjectPoolGetResult): string;
begin
  case gr of
    grSuccess: Result := 'Success';
    grReuse: Result := 'Reuse';
    grReinitFail: Result := 'InitFail';
    grGetFail: Result := 'GetFail';
    grGetError: Result := 'GetError';
    grWaitFail: Result := 'WaitFail';
  else
    Result := 'Unknown';
  end;
end;

procedure TForm1.CnThreadPool2ProcessRequest(Sender: TCnThreadPool;
  aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
var
  con: TADOConnection;
  //i: Integer;
begin
  if Application.Terminated then
    Exit;

  try
    CoInitialize(nil);
    try
      Output(grToStr(CnADOConPool1.GetConnection(con, TCnObjectPoolGetOption(ComboBox7.ItemIndex))));
      {
      for i := 0 to Random(9) do
      begin
        if Application.Terminated then
          Exit;

        Sleep(100);
      end;
      //}
      if con <> nil then
      begin
        try
          Output('RecordCount=' + IntToStr(con.Execute(Edit1.Text).RecordCount));
        except
          on E: Exception do
            Output('Error: ' + E.Message);
        end;
        CnADOConPool1.ReleaseConnection(con);
      end;
    finally
      CoUninitialize;
    end;

  except
  end;
  
end;

procedure TForm1.CnThreadPool1ProcessRequest(Sender: TCnThreadPool;
  aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread);
var
  obj: TObject;
  i: Integer;
begin
  if Application.Terminated then
    Exit;

  i := ComboBox8.ItemIndex;
  if (i < 0) or (i > 2) then
    i := Random(3);
  Output(grToStr(CnObjectPool1.GetObject(obj, TCnObjectPoolGetOption(i))));
  //grToStr(CnObjectPool1.GetObject(obj, TCnObjectPoolGetOption(i)));
  //{
  for i := 0 to Random(9) do
  begin
    if Application.Terminated then
      Exit;

    Sleep(100);
  end;
  //}
  if obj <> nil then
    CnObjectPool1.ReleaseObject(obj);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Randomize;

  cs := TCriticalSection.Create;
  sslog := TStringList.Create;

  iInTimer := 0;
  CnObjectPool1.ObjectClass := TObject;

  ComboBox1.ItemIndex := Integer(CnObjectPool1.PolicyOnGet);
  ComboBox2.ItemIndex := Integer(CnObjectPool1.PolicyOnBusy);
  ComboBox3.ItemIndex := Integer(CnObjectPool1.PolicyOnPeak);
  SpinEdit1.Value := CnObjectPool1.PeakCount;
  SpinEdit2.Value := CnObjectPool1.MinSize;
  SpinEdit3.Value := CnObjectPool1.MaxSize;

  ComboBox4.ItemIndex := Integer(CnADOConPool1.PolicyOnGet);
  ComboBox5.ItemIndex := Integer(CnADOConPool1.PolicyOnBusy);
  ComboBox6.ItemIndex := Integer(CnADOConPool1.PolicyOnPeak);
  SpinEdit5.Value := CnADOConPool1.PeakCount;
  SpinEdit7.Value := CnADOConPool1.MinSize;
  SpinEdit8.Value := CnADOConPool1.MaxSize;
  SpinEdit11.Value := CnADOConPool1.LowLoadCount;
  Edit2.Text := CnADOConPool1.ConnectionString;

  PageControl1.ActivePageIndex := 0;

  for i := 0 to ControlCount - 1 do
    if Controls[i] is TCustomMemo then
      TCustomMemo(Controls[i]).Clear;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to SpinEdit4.Value - 1 do
    CnThreadPool1.AddRequest(TCnTaskDataObject.Create, []);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to SpinEdit9.Value - 1 do
    CnThreadPool2.AddRequest(TCnTaskDataObject.Create, []);
end;

procedure TForm1.Output(const s: string);
begin
  if not CheckBox3.Checked then
    Exit;

  cs.Enter;
  try
    sslog.Add(s);
    //Memo5.Items.BeginUpdate;
    //Memo5.Items.Add(s);
    //Memo5.Items.EndUpdate;
    //NeedSelectItem := True;
  finally
    cs.Leave;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CnADOConPool1.ConnectionString := PromptDataSource(Handle, CnADOConPool1.ConnectionString);
  Edit2.Text := CnADOConPool1.ConnectionString;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  CnADOConPool1.PolicyOnGet := TCnObjectPoolGetPolicy(ComboBox4.ItemIndex);
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  CnADOConPool1.PolicyOnBusy := TCnObjectPoolBusyPolicy(ComboBox5.ItemIndex);
end;

procedure TForm1.ComboBox6Change(Sender: TObject);
begin
  CnADOConPool1.PolicyOnPeak := TCnObjectPoolPeakPolicy(ComboBox6.ItemIndex);
end;

procedure TForm1.SpinEdit7Change(Sender: TObject);
begin
  CnADOConPool1.MinSize := SpinEdit7.Value;
end;

procedure TForm1.SpinEdit8Change(Sender: TObject);
begin
  CnADOConPool1.MaxSize := SpinEdit8.Value;
end;

procedure TForm1.SpinEdit5Change(Sender: TObject);
begin
  CnADOConPool1.PeakCount := SpinEdit5.Value;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  CnObjectPool1.PolicyOnGet := TCnObjectPoolGetPolicy(ComboBox1.ItemIndex);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  CnObjectPool1.PolicyOnBusy := TCnObjectPoolBusyPolicy(ComboBox2.ItemIndex);
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  CnObjectPool1.PolicyOnPeak := TCnObjectPoolPeakPolicy(ComboBox3.ItemIndex);
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  CnObjectPool1.MinSize := SpinEdit2.Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
  CnObjectPool1.MaxSize := SpinEdit3.Value;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  CnObjectPool1.PeakCount := SpinEdit1.Value;
end;

procedure TForm1.CnADOConPool1ReInitOne(Pool: TCnCustomObjectPool;
  Wrapper: TCnObjectWrapper; var Obj: TObject; var bSuccess: Boolean);
begin
  Output('On Reinit !');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sslog.Free;
  cs.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Application.Terminate;
  CreateTerminateThread(5);
  Timer1.Enabled := False;
  FreeAndNil(CnThreadPool1);
  FreeAndNil(CnThreadPool2);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Memo5.Clear;
end;

procedure TForm1.SpinEdit11Change(Sender: TObject);
begin
  CnADOConPool1.LowLoadCount := SpinEdit11.Value;
end;

end.
