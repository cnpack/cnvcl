{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
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

unit CnCodeDemo;
{* |<PRE>
================================================================================
* ������ƣ�CnPack�����
* ��Ԫ���ƣ��������淶ʾ����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: Delphi��Ԫ�淶��ʽ.pas,v 1.8 2009/02/25 12:32:57 liuxiao Exp $
* ��    ע��- �˵�Ԫ����ֻ��Ϊ CnPack �Ĵ���淶ʾ����Ԫ�����Ķ���Ա��ã�������
*             ʵ�ʵı�������ԡ�
*           - �����е�TCnTimer���õ������߳̽��ж�ʱ���ƣ����ȱ�TTimerҪ�ߣ���Ӧ
*             ��Ҳռ�ý϶��CPU��Դ��
* �޸ļ�¼��2009.02.18 V1.1
*               ���ĵ�Ԫ˵��
*           2002.04.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, SysUtils, ExtCtrls, CnClasses, CnConsts, CnCompConsts;

type

//==============================================================================
// �߾��ȶ�ʱ�������ʱ�߳�
//==============================================================================

{ TCnTimerThread }

  TCnTimer = class;

  TCnTimerThread = class(TThread)
  private
    FOwner: TCnTimer;
    FInterval: Word;
    FStop: THandle;
  protected
    constructor Create(CreateSuspended: Boolean); virtual;
    procedure Execute; override;
  end;

//==============================================================================
// �߾��ȶ�ʱ�����
//==============================================================================

{ TCnTimer }

  TTimerQuality = (tqHighest, tqHigh, tqLow);
  {* �߾��ȶ�ʱ����ʱ��������
   |<PRE>
     tqHighest  - ��߾��ȣ����ø����ȼ����̶߳�ʱ
     tqHigh     - �߾��ȣ�������ͨ���ȼ����̶߳�ʱ
     tqLow      - �;��ȣ��ڲ�ʹ��TTimer���ж�ʱ
   |</PRE>}

  TCnTimer = class(TCnComponent)
  {* �߾��ȶ�ʱ�������ʹ�õ������߳̽��ж�ʱ���ƣ�ʹ�÷�����TTimerһ����
     ��������һ��Quality���Կ��ƶ�ʱ����}
  private
    FOnTimer: TNotifyEvent;
    FQuality: TTimerQuality;
    FEnabled: Boolean;
    FInterval: Word;
    FTimerThread: TCnTimerThread;
    FTimer: TTimer;
    FLastTick: Cardinal;
    FLastCountTick: Cardinal;
    FActualInterval: Integer;
    FActualRate: Integer;
    FCount: Integer;
    procedure DoTimer;
    procedure OnTimerTimer(Sender: TObject);
    procedure CreateTimer;
    procedure CreateTimerThread;
    procedure FreeTimer;
    procedure FreeTimerThread;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure SetQuality(const Value: TTimerQuality);
  protected
    function GetAuthor: string; override;
    function GetComment: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActualInterval: Integer read FActualInterval;
    {* ʵ�ʵĶ�ʱ�������λΪ����}
    property ActualRate: Integer read FActualRate;
    {* ʵ�ʵĶ�ʱ�ٶȣ���λΪ��ÿ��}
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    {* �Ƿ�����ʱ�¼�}
    property Interval: Word read FInterval write SetInterval default 1000;
    {* ��ʱ�������λΪ����}
    property Quality: TTimerQuality read FQuality write SetQuality default tqLow;
    {* ��ʱ���ȣ����IntervalС��55��Win9X����10��WinNT����������Ϊ�߾�������}
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    {* ��ʱ���¼�}
  end;

implementation

//==============================================================================
// �߾��ȶ�ʱ�������ʱ�߳�
//==============================================================================

{ TCnTimerThread }

// ��ʼ���߳�
constructor TCnTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FStop := CreateEvent(nil, False, False, nil); // �����˳����¼�
end;

// �߳�����
procedure TCnTimerThread.Execute;
begin
  repeat                      // �ȴ��˳��¼���λ�� FInterval �����ʱ�˳�
    if WaitForSingleObject(FStop, FInterval) = WAIT_TIMEOUT then
      Synchronize(FOwner.DoTimer); // ͬ����ʽ������ʱ�¼�
  until Terminated;
  CloseHandle(FStop);         // �ͷ��¼����
end;

{ TCnTimer }

//==============================================================================
// �߾��ȶ�ʱ�����
//==============================================================================

// �����ʼ��
constructor TCnTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FInterval := 1000;
  FQuality := tqLow;
  FTimer := nil;
  FTimerThread := nil;
  CreateTimer;
end;

// �ͷ�
destructor TCnTimer.Destroy;
begin
  FreeTimer;
  FreeTimerThread;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// �¼�����
//------------------------------------------------------------------------------

// ������ʱ�¼�
procedure TCnTimer.DoTimer;
var
  Tick: Cardinal;
begin
  Tick := GetTickCount;
  if (FLastTick = 0) and (FLastCountTick = 0) then
  begin
    FLastTick := Tick;
    FLastCountTick := Tick;
  end
  else
  begin
    FActualInterval := Tick - FLastTick;
    FLastTick := Tick;
    if Tick - FLastCountTick >= 1000 then
    begin
      FActualRate := FCount;
      FLastCountTick := Tick;
      FCount := 0;
    end else
      Inc(FCount);
  end;
  begin
    if Assigned(FOnTimer) then
      FOnTimer(Self);
  end;
end;

//------------------------------------------------------------------------------
// �ڲ���ʱ�������ͷ�
//------------------------------------------------------------------------------

// �ڲ�Timer�¼�
procedure TCnTimer.OnTimerTimer(Sender: TObject);
begin
  DoTimer;
end;

// �����ڲ�Timer��ʱ�����;��ȣ�
procedure TCnTimer.CreateTimer;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := OnTimerTimer;
    FTimer.Interval := FInterval;
    FTimer.Enabled := FEnabled;
  end;
end;

// ������ʱ���̣߳��߾��ȣ�
procedure TCnTimer.CreateTimerThread;
begin
  if not Assigned(FTimerThread) then
  begin
    FTimerThread := TCnTimerThread.Create(True);
    FTimerThread.FOwner := Self;
    FTimerThread.FreeOnTerminate := False;
    FTimerThread.Priority := tpNormal;
    FTimerThread.FInterval := FInterval;
    if FEnabled then
    begin
      if FInterval > 0 then
      begin
        SetEvent(FTimerThread.FStop);
        FTimerThread.Resume;
      end;
    end
    else
      FTimerThread.Suspend;
  end;
end;

// �ͷ��ڲ���ʱ�����;��ȣ�
procedure TCnTimer.FreeTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
end;

// �ͷŶ�ʱ���̣߳��߾��ȣ�
procedure TCnTimer.FreeTimerThread;
begin
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    SetEvent(FTimerThread.FStop);
    if FTimerThread.Suspended then FTimerThread.Resume;
    FTimerThread.WaitFor;
    FTimerThread.Free;
    FTimerThread := nil;
  end;
end;

//------------------------------------------------------------------------------
// ���Զ�д����
//------------------------------------------------------------------------------

// ���ö�ʱ����
procedure TCnTimer.SetQuality(const Value: TTimerQuality);
begin
  if FQuality <> Value then
  begin
    FQuality := Value;
    case FQuality of
      tqHighest, tqHigh:
        begin
          FreeTimer;
          CreateTimerThread;
          if Value = tqHighest then
            FTimerThread.Priority := tpHigher
          else
            FTimerThread.Priority := tpNormal;
        end;
      tqLow:
        begin
          FreeTimerThread;
          CreateTimer;
        end;
    end;
  end;
end;

// �����Ƿ�����ʱ
procedure TCnTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FQuality = tqLow then
      FTimer.Enabled := FEnabled
    else
    begin
      if FEnabled then
      begin
        if FTimerThread.FInterval > 0 then
        begin
          SetEvent(FTimerThread.FStop);
          FTimerThread.Resume;
        end;
      end
      else
        FTimerThread.Suspend;
    end;
  end;
end;

// ���ö�ʱ���
procedure TCnTimer.SetInterval(Value: Word);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    Enabled := False;
    if FQuality = tqLow then
      FTimer.Interval := FInterval
    else
      FTimerThread.FInterval := FInterval;
    Enabled := True;
  end;
end;

// ȡ�������
function TCnTimer.GetAuthor: string;
begin
  Result := SCnPack_Zjy;
end;

// ȡ���ע��
function TCnTimer.GetComment: string;
begin
  Result := SCnTimerComment;
end;

end.

