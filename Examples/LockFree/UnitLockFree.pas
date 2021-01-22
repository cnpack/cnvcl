unit UnitLockFree;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SyncObjs, CnLockFree, ExtCtrls;

type
  TFormLockFree = class(TForm)
    btnTestNoCritical: TButton;
    btnTestCritical: TButton;
    btnTestSysCritical: TButton;
    btnTestLockFreeLinkedList: TButton;
    btnTestLockFreeInsert: TButton;
    btnLockFreeLinkedListInsert: TButton;
    mmoLinkedListResult: TMemo;
    btnLockFreeLinkedListInsertDelete: TButton;
    btnTestIncDec: TButton;
    rgIncDec: TRadioGroup;
    btnTestSingleRing: TButton;
    procedure btnTestNoCriticalClick(Sender: TObject);
    procedure btnTestCriticalClick(Sender: TObject);
    procedure btnTestSysCriticalClick(Sender: TObject);
    procedure btnTestLockFreeLinkedListClick(Sender: TObject);
    procedure btnTestLockFreeInsertClick(Sender: TObject);
    procedure btnLockFreeLinkedListInsertClick(Sender: TObject);
    procedure btnLockFreeLinkedListInsertDeleteClick(Sender: TObject);
    procedure btnTestIncDecClick(Sender: TObject);
    procedure btnTestSingleRingClick(Sender: TObject);
  private
    procedure NoCriticalTerminate(Sender: TObject);
    procedure CriticalTerminate(Sender: TObject);
    procedure SysCriticalTerminate(Sender: TObject);
    procedure LinkedListTerminate(Sender: TObject);
    procedure LinkedListInsertTerminate(Sender: TObject);
    procedure LinkedListDeleteTerminate(Sender: TObject);
    procedure TravelNode(Sender: TObject; Node: PCnLockFreeLinkedNode);
    procedure IncDecTerminate(Sender: TObject);
    procedure SingleQueueTerminate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormLockFree: TFormLockFree;

implementation

{$R *.DFM}

type
  TNoCriticalThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TCriticalThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TSysCriticalThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TLinkedListAppendThread = class(TThread)
  private
    FBase: Integer;
  protected
    procedure Execute; override;
  public
    property Base: Integer read FBase write FBase;
  end;

  TLinkedListInsertThread = class(TThread)
  private
    FBase: Integer;
  protected
    procedure Execute; override;
  public
    property Base: Integer read FBase write FBase;
  end;

  TLinkedListDeleteThread = class(TThread)
  private
    FBase: Integer;
  protected
    procedure Execute; override;
  public
    property Base: Integer read FBase write FBase;
  end;

  TIncDecThread = class(TThread)
  private
    FMode: Integer; // 0, 1, 2, 3 分别代表 Int32 + 1, Int32 - 1, Int64 + 1, Int64 - 1;
  protected
    procedure Execute; override;
  public
    property Mode: Integer read FMode write FMode;
  end;

  TSingleQueueReadThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TSingleQueueWriteThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  NoCriticalValue: Integer;
  NoCriticalTerminateCount: Integer;

  CS: TCriticalSection;

  CO: TCnSpinLockRecord;
  CriticalValue: Integer;
  CriticalTerminateCount: Integer;

  FLink: TCnLockFreeLinkedList;
  LinkTerminateCount: Integer;

  FCas32: Integer;
  FCas64: Int64;
  CasTerminateCount: Integer;

  FSingleQueue: TCnLockFreeSingleRingQueue;
  SingleQueueTerminateCount: Integer;

procedure TFormLockFree.btnTestNoCriticalClick(Sender: TObject);
var
  T1, T2, T3: TNoCriticalThread;
begin
  NoCriticalValue := 0;
  NoCriticalTerminateCount := 0;

  T1 := TNoCriticalThread.Create(True);
  T2 := TNoCriticalThread.Create(True);
  T3 := TNoCriticalThread.Create(True);
  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := NoCriticalTerminate;
  T2.OnTerminate := NoCriticalTerminate;
  T3.OnTerminate := NoCriticalTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

{ TNoCriticalThread }

procedure TNoCriticalThread.Execute;
var
  I, Old: Integer;
begin
  for I := 1 to 1000 do
  begin
    Old := NoCriticalValue;
    Sleep(0);
    NoCriticalValue := Old + 1;
  end;
end;

procedure TFormLockFree.NoCriticalTerminate(Sender: TObject);
begin
  NoCriticalTerminateCount := NoCriticalTerminateCount + 1;

  if NoCriticalTerminateCount >= 3 then
    ShowMessage(IntToStr(NoCriticalValue) + ' Terminated: ' + IntToStr(NoCriticalTerminateCount));
end;

{ TCriticalThread }

procedure TCriticalThread.Execute;
var
  I, Old: Integer;
begin
  for I := 1 to 10000 do
  begin
    CnSpinLockEnter(CO);
    Old := CriticalValue;
    Sleep(0);
    CriticalValue := Old + 1;
    CnSpinLockLeave(CO);
  end;
end;

procedure TFormLockFree.btnTestCriticalClick(Sender: TObject);
var
  T1, T2, T3: TCriticalThread;
begin
  CriticalValue := 0;
  CriticalTerminateCount := 0;
  CnInitSpinLockRecord(CO);

  T1 := TCriticalThread.Create(True);
  T2 := TCriticalThread.Create(True);
  T3 := TCriticalThread.Create(True);
  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := CriticalTerminate;
  T2.OnTerminate := CriticalTerminate;
  T3.OnTerminate := CriticalTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

procedure TFormLockFree.CriticalTerminate(Sender: TObject);
begin
  CriticalTerminateCount := CriticalTerminateCount + 1;

  if CriticalTerminateCount >= 3 then
    ShowMessage(IntToStr(CriticalValue) + ' Terminated: ' + IntToStr(CriticalTerminateCount));
end;

{ TSysCriticalThread }

procedure TSysCriticalThread.Execute;
var
  I, Old: Integer;
begin
  for I := 1 to 10000 do
  begin
    CS.Enter;
    Old := CriticalValue;
    Sleep(0);
    CriticalValue := Old + 1;
    CS.Leave;
  end;
end;

procedure TFormLockFree.btnTestSysCriticalClick(Sender: TObject);
var
  T1, T2, T3: TSysCriticalThread;
begin
  CriticalValue := 0;
  CriticalTerminateCount := 0;
  CS := TCriticalSection.Create;

  T1 := TSysCriticalThread.Create(True);
  T2 := TSysCriticalThread.Create(True);
  T3 := TSysCriticalThread.Create(True);
  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := SysCriticalTerminate;
  T2.OnTerminate := SysCriticalTerminate;
  T3.OnTerminate := SysCriticalTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

procedure TFormLockFree.SysCriticalTerminate(Sender: TObject);
begin
  CriticalTerminateCount := CriticalTerminateCount + 1;

  if CriticalTerminateCount >= 3 then
  begin
    ShowMessage(IntToStr(CriticalValue) + ' Terminated: ' + IntToStr(CriticalTerminateCount));
    CS.Free;
  end;
end;

procedure TFormLockFree.btnTestLockFreeLinkedListClick(Sender: TObject);
var
  T1, T2, T3: TLinkedListAppendThread;
begin
  FLink := TCnLockFreeLinkedList.Create;
  LinkTerminateCount := 0;

  T1 := TLinkedListAppendThread.Create(True);
  T2 := TLinkedListAppendThread.Create(True);
  T3 := TLinkedListAppendThread.Create(True);
  T1.Base := 0;
  T2.Base := 10000;
  T3.Base := 20000;

  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := LinkedListTerminate;
  T2.OnTerminate := LinkedListTerminate;
  T3.OnTerminate := LinkedListTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

{ TLinkedListThread }

procedure TLinkedListAppendThread.Execute;
var
  I: Integer;
begin
  for I := 1 to 1000 do
  begin
    Sleep(0);
    FLink.Append(TObject(I + FBase), nil);
  end;
end;

procedure TFormLockFree.LinkedListTerminate(Sender: TObject);
begin
  LinkTerminateCount := LinkTerminateCount + 1;

  if LinkTerminateCount >= 3 then
  begin
    ShowMessage(IntToStr(FLink.GetCount) + ' Terminated: ' + IntToStr(LinkTerminateCount));
    mmoLinkedListResult.Lines.Clear;
    FLink.OnTravelNode := TravelNode;
    FLink.Travel;
    FLink.Free;
  end;
end;

procedure TFormLockFree.btnTestLockFreeInsertClick(Sender: TObject);
var
  I: Integer;
  List: TCnLockFreeLinkedList;
begin
  List := TCnLockFreeLinkedList.Create;
  for I := 1 to 10 do
    List.Insert(TObject(I * 3), nil);
  for I := 1 to 10 do
    List.Insert(TObject(I * 3 + 1), nil);

  mmoLinkedListResult.Lines.Clear;
  List.OnTravelNode := TravelNode;
  ShowMessage(IntToStr(List.GetCount));
  List.Travel;

  List.Delete(TObject(6));
  List.Travel;
  List.Free;
end;

procedure TFormLockFree.TravelNode(Sender: TObject;
  Node: PCnLockFreeLinkedNode);
begin
  mmoLinkedListResult.Lines.Add(IntToStr(Integer(Node^.Key)));
end;

procedure TFormLockFree.btnLockFreeLinkedListInsertClick(Sender: TObject);
var
  T1, T2, T3: TLinkedListInsertThread;
begin
  FLink := TCnLockFreeLinkedList.Create;
  LinkTerminateCount := 0;

  T1 := TLinkedListInsertThread.Create(True);
  T2 := TLinkedListInsertThread.Create(True);
  T3 := TLinkedListInsertThread.Create(True);
  T1.Base := 0;
  T2.Base := 1;
  T3.Base := 2;

  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := LinkedListTerminate;
  T2.OnTerminate := LinkedListTerminate;
  T3.OnTerminate := LinkedListTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

{ TLinkedListInsertThread }

procedure TLinkedListInsertThread.Execute;
var
  I: Integer;
begin
  for I := 1 to 1000 do
  begin
    Sleep(0);
    FLink.Insert(TObject(I * 3 + FBase), nil);
  end;
end;

procedure TFormLockFree.btnLockFreeLinkedListInsertDeleteClick(
  Sender: TObject);
var
  T1, T2, T3: TLinkedListInsertThread;
begin
  FLink := TCnLockFreeLinkedList.Create;
  LinkTerminateCount := 0;

  T1 := TLinkedListInsertThread.Create(True);
  T2 := TLinkedListInsertThread.Create(True);
  T3 := TLinkedListInsertThread.Create(True);
  T1.Base := 0;
  T2.Base := 1;
  T3.Base := 2;

  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := LinkedListInsertTerminate;
  T2.OnTerminate := LinkedListInsertTerminate;
  T3.OnTerminate := LinkedListInsertTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

procedure TFormLockFree.LinkedListInsertTerminate(Sender: TObject);
var
  T1, T2, T3: TLinkedListDeleteThread;
begin
  LinkTerminateCount := LinkTerminateCount + 1;

  if LinkTerminateCount >= 3 then
  begin
    ShowMessage(IntToStr(FLink.GetCount) + ' Terminated: ' + IntToStr(LinkTerminateCount));
    mmoLinkedListResult.Lines.Clear;
    FLink.OnTravelNode := TravelNode;
    FLink.Travel;

    LinkTerminateCount := 0;

    T1 := TLinkedListDeleteThread.Create(True);
    T2 := TLinkedListDeleteThread.Create(True);
    T3 := TLinkedListDeleteThread.Create(True);
    T1.Base := 0;
    T2.Base := 1;
    T3.Base := 2;

    T1.FreeOnTerminate := True;
    T2.FreeOnTerminate := True;
    T3.FreeOnTerminate := True;
    T1.OnTerminate := LinkedListDeleteTerminate;
    T2.OnTerminate := LinkedListDeleteTerminate;
    T3.OnTerminate := LinkedListDeleteTerminate;
    T1.Resume;
    T2.Resume;
    T3.Resume;
  end;
end;

{ TLinkedListDeleteThread }

procedure TLinkedListDeleteThread.Execute;
var
  I: Integer;
begin
  for I := 1 to 1000 do
  begin
    Sleep(0);
    FLink.Delete(TObject(I * 3 + FBase));
  end;
end;

procedure TFormLockFree.LinkedListDeleteTerminate(Sender: TObject);
begin
  LinkTerminateCount := LinkTerminateCount + 1;

  if LinkTerminateCount >= 3 then
  begin
    ShowMessage(IntToStr(FLink.GetCount) + ' Terminated: ' + IntToStr(LinkTerminateCount));
    FLink.Free;
  end;
end;

{ TIncDecThread }

procedure TIncDecThread.Execute;
var
  I: Integer;
begin
  for I := 1 to 1000 do
  begin
    case FMode of
      0: CnAtomicIncrement32(FCas32);
      1: CnAtomicDecrement32(FCas32);
      2: CnAtomicIncrement64(FCas64);
      3: CnAtomicDecrement64(FCas64);
    end;
  end;
end;

procedure TFormLockFree.btnTestIncDecClick(Sender: TObject);
var
  T1, T2, T3: TIncDecThread;
begin
  FCas32 := 5000;
  FCas64 := 5000000;
  CasTerminateCount := 0;

  T1 := TIncDecThread.Create(True);
  T2 := TIncDecThread.Create(True);
  T3 := TIncDecThread.Create(True);
  T1.Mode := rgIncDec.ItemIndex;
  T2.Mode := rgIncDec.ItemIndex;
  T3.Mode := rgIncDec.ItemIndex;

  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T3.FreeOnTerminate := True;
  T1.OnTerminate := IncDecTerminate;
  T2.OnTerminate := IncDecTerminate;
  T3.OnTerminate := IncDecTerminate;
  T1.Resume;
  T2.Resume;
  T3.Resume;
end;

procedure TFormLockFree.IncDecTerminate(Sender: TObject);
begin
  Inc(CasTerminateCount);
  if CasTerminateCount >= 3 then
  begin
    case rgIncDec.ItemIndex of
      0, 1: ShowMessage(IntToStr(FCas32));
      2, 3: ShowMessage(IntToStr(FCas64));
    end;
  end;
end;

procedure TFormLockFree.btnTestSingleRingClick(Sender: TObject);
var
  T1: TSingleQueueReadThread;
  T2: TSingleQueueWriteThread;
begin
  FSingleQueue := TCnLockFreeSingleRingQueue.Create(128);
  SingleQueueTerminateCount := 0;

  T1 := TSingleQueueReadThread.Create(True);
  T2 := TSingleQueueWriteThread.Create(True);
  T1.FreeOnTerminate := True;
  T2.FreeOnTerminate := True;
  T1.OnTerminate := SingleQueueTerminate;
  T2.OnTerminate := SingleQueueTerminate;
  T1.Resume;
  T2.Resume;
end;

{ TSingleQueueWriteThread }

procedure TSingleQueueWriteThread.Execute;
var
  I: Integer;
begin
  for I := 1 to 1000 do
  begin
    while not FSingleQueue.Enqueue(TObject(I), nil) do
      ;
  end;
end;

{ TSingleQueueReadThread }

procedure TSingleQueueReadThread.Execute;
var
  I: Integer;
  K, V: TObject;
begin
  for I := 1 to 1000 do
  begin
    while not FSingleQueue.Dequeue(K, V) do
      ;
  end;
end;

procedure TFormLockFree.SingleQueueTerminate(Sender: TObject);
begin
  Inc(SingleQueueTerminateCount);
  if SingleQueueTerminateCount >= 2 then
  begin
    ShowMessage(IntToStr(FSingleQueue.Count));
    FSingleQueue.Free;
  end;
end;

end.
