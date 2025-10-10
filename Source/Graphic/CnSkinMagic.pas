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

unit CnSkinMagic;
{* |<PRE>
================================================================================
* ������ƣ������ӹ����������Ԫ
* ��Ԫ���ƣ������ڻ�Ƥ����ܣ�Ƥ��Ч������ʵ��
* ��Ԫ���ߣ�CnPack ������ savetime
            (savetime2k@hotmail.com, http://savetime.delphibbs.com)
* ��    ע������Ԫ��ԭ������Ȩ CnPack ��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.09.25 V1.1
*                ֧�� Win64
*           2007.07.27 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  CnNative, CnClasses, CnConsts, CnCompConsts, CnGraphConsts;

type

  TControlSubClass = class(TObject)
  {* �ؼ��� SubClass ֮��������Ķ���Skin �������Ҫ�˽� public �������}
  private
    FControl: TControl;
    FSkinWindowProc: TWndMethod;
    FOldWindowProc: TWndMethod;
    FIsWinControl: Boolean;
    FMouseInControl: Boolean;
    constructor Create(AControl: TControl);
    {* Skin ���������������,���Խ������� private ��}
    procedure WindowProc(var Message: TMessage);
  public
    property Control: TControl read FControl;
    {* �� SubClass ��Ŀؼ�(ֻ��) }
    property OldWindowProc: TWndMethod read FOldWindowProc;
    {* �ؼ�ԭʼ�� WindowProc }
    property IsWinControl: Boolean read FIsWinControl;
    {* ��ǰ�ؼ��Ƿ��� TWinControl ���� }
    property MouseInControl: Boolean read FMouseInControl;
    {* ����Ƿ��ڵ�ǰ�ؼ��� }    
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnSkinMagic = class(TCnComponent)
  {* �����ڻ�Ƥ��������ɲ�ʵ������ֱ��ʹ�����෽�� }
  private
    function GetSkinActive: Boolean;
  protected
    class procedure RefreshControls;
    {* ����ˢ�����пؼ�}
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    class procedure EnableSkin;
    {* ʹ�� CnSkinMagic �Ĵ��ڹ��̹ҽ� }
    class procedure DisableSkin;
    {* ���� CnSkinMagic �Ĵ��ڹ��̹ҽ� }
    class procedure RegisterClass(AClass: TControlClass; AWindowProc: Pointer);
    {* ע����Ҫ�ҽӵ� CnSkinMagic Class, ���ע��ʧ��, �������쳣
       ����: AClass    �� SubClass ����, ����Ϊ TControlClass ������
             AWndProc  SubClass ��� WindowProc, ���û����
       ע�⣺AWindowProc ����������µĸ�ʽ��
       procedure AWindowProc(Self: TControlSubClass; var Message: TMessage);
    }
  published
    property SkinActive: Boolean read GetSkinActive;
    {* ���ص�ǰ Skin �Ƿ񼤻�, �� EnableSkin �� DisableSkin ����}
  end;

{==============================================================================}
{ IMPLEMENTATION - �����Ǿ���ʵ������, Skin ����߿��Բ������´���             }
{==============================================================================}

implementation

type
  TAfterConstruction = procedure(Self: Pointer);
  TBeforeDestruction = procedure(Self: Pointer);
  // TObject.AfterConstruction & BeforeDestruction ��������

  TCnClassList = class(TList)
  {* ���ڹ����û������ ClassData }
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  PClassData = ^TClassData;
  TClassData = record
  {* TCnClassList �е������� }
    ClassType: TClass;
    WindowProc: Pointer;
    OldAfterConstruction: Pointer;
    OldBeforeDestruction: Pointer;
  end;

var
  FClassList: TCnClassList;
  {* (ȫ��)�û����������Ϣ(TClassData)�б� }
  FSkinActive: Boolean;
  {* ��ǰ Skin �Ƿ񼤻�, �� EnableSkin �� DisableSkin  ���� }
  CN_MSG_BEFORE_DESTRUCTION: Cardinal;
  {* CN_MSG_BEFORE_DESTRUCTION ��Ϣ, ֪ͨ TControlSubClass ��������}

{==============================================================================}
{ �����Ǹ�������                                                               }
{==============================================================================}

{------------------------------------------------------------------------------}
// FindClassData - �� FClassList ���ҵ��ؼ��� TClassData ��Ϣ
{------------------------------------------------------------------------------}
function FindClassData(AClass: TClass): PClassData;
var
  I: Integer;
begin
  for I := 0 to FClassList.Count - 1 do
  begin
    Result := PClassData(FClassList.Items[I]);
    if AClass = Result.ClassType then
      Exit;
  end;
  raise Exception.Create(SCNE_FINDCLASSDATAFAILED);
end;

{------------------------------------------------------------------------------}
// MakeMethod - ��һ�㾲̬����ת��Ϊ TMethod ����
{------------------------------------------------------------------------------}
function MakeMethod(Self: Pointer; FuncAddr: Pointer): TMethod;
begin
  Result.Code := FuncAddr;
  Result.Data := Self;
end;

{------------------------------------------------------------------------------}
// CnAfterConstruction - ���滻�� AfterConstruction ����, ���ڿؼ�����������
// �˺��������ú󴴽� TControlSubClass �������¹���Ŀؼ�������һ��
// ��󻹵�����ԭʼ�� AfterConstruction ����
{------------------------------------------------------------------------------}
procedure CnAfterConstruction(Self: TControl);
var
  OldAfterConstruction: TAfterConstruction;
  ClassDataPtr: PClassData;
begin
  // �½�һ�� ControlSubClass ����, ���ڿ��Ƶ�ǰ�ؼ�����Ϊ
  TControlSubClass.Create(Self);

  // �ҵ���ǰ�ؼ���(�û������)������
  ClassDataPtr := FindClassData(Self.ClassType);
  // ����(�û������)�������г�ʼ�� AfterConstruction ����
  OldAfterConstruction := ClassDataPtr^.OldAfterConstruction;
  // ����� AfterConstruction ����, ��ִ����
  if Assigned(OldAfterConstruction) then OldAfterConstruction(Self);
end;

{------------------------------------------------------------------------------}
// CnBeforeDestruction - ���滻�� BeforeDestruction ����, ���ڿؼ�������ǰ����
// �˺��������ú�����Ϣ�� TControlSubClass ����, ʹ֮ͬʱ����
// ��󻹵�����ԭʼ�� BeforeDestruction ����
{------------------------------------------------------------------------------}
procedure CnBeforeDestruction(Self: TControl);
var
  OldBeforeDestruction: TBeforeDestruction;
  ClassDataPtr: PClassData;
begin
  // ������Ϣ֪ͨ TControlSubClass ����
  Self.Perform(CN_MSG_BEFORE_DESTRUCTION, 0, 0);

  // �ҵ���ǰ�ؼ���(�û������)������
  ClassDataPtr := FindClassData(Self.ClassType);
  // ����(�û������)�������г�ʼ�� BeforeDestruction ����
  OldBeforeDestruction := ClassDataPtr^.OldBeforeDestruction;
  // ����� BeforeDestruction ����, ��ִ����
  if Assigned(OldBeforeDestruction) then
    OldBeforeDestruction(Self);
end;

{------------------------------------------------------------------------------}
// WriteVmtPtr - ��д vmt ָ��ֵ (д�����)
{------------------------------------------------------------------------------}
procedure WriteVmtPtr(VmtPtrAddr: Pointer; AFuncAddr: Pointer);
var
  ProcessHandle: THandle;
  WriteBytesCount: TCnNativeUInt;
begin
  ProcessHandle := OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_WRITE,
    False, GetCurrentProcessId());
  {TODO: �ǲ���Ҫ���һ�� ProcessHandle }

  WriteProcessMemory(ProcessHandle, VmtPtrAddr, @AFuncAddr, 4, WriteBytesCount);

  CloseHandle(ProcessHandle);

  if WriteBytesCount <> 4 then
    raise Exception.Create(SCNE_WRITEVMTFAILED);
end;

{------------------------------------------------------------------------------}
// TCnClassList.Notify - �� Item ��ɾ��ʱ�Զ����� TClassData ������
{------------------------------------------------------------------------------}
procedure TCnClassList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    Dispose(Ptr);
end;

{------------------------------------------------------------------------------}
// TControlSubClass.Create - ���� SubClass ������ؼ�������һ��
{------------------------------------------------------------------------------}
constructor TControlSubClass.Create(AControl: TControl);
var
  ClassDataPtr: PClassData;
begin
  // �ҵ���ǰ�ؼ���(�û������)������
  ClassDataPtr := FindClassData(AControl.ClassType);
  // �� ControlSubClass �б��浱ǰ�ؼ�ָ��
  FControl := AControl;
  // �� ControlSubClass �д洢��ǰ�ؼ��Ƿ��� TWinControl
  FIsWinControl := AControl is TWinControl;
  // �ϳ� FSkinWindowProc;
  FSkinWindowProc := TWndMethod(MakeMethod(Self, ClassDataPtr^.WindowProc));
  // �� ControlSubClass �б��浱ǰ�ؼ��Ĵ��ڹ���
  FOldWindowProc := AControl.WindowProc;
  // ���õ�ǰ����Ĵ��ڹ���Ϊ(�û������)�������еĴ��ڹ���
  AControl.WindowProc := Self.WindowProc;
end;

{------------------------------------------------------------------------------}
// TControlSubClass.WindowProc - �� SubClass ʱ��һ�������õ� WindowProc
{------------------------------------------------------------------------------}
procedure TControlSubClass.WindowProc(var Message: TMessage);
begin
  if Message.Msg = CN_MSG_BEFORE_DESTRUCTION then  // �ؼ�������
  begin
    Destroy;
    Exit;
  end;

  if not FSkinActive then
  begin
    OldWindowProc(Message);
    Exit;
  end;

  case Message.Msg of
    CM_MOUSEENTER:  // ������ؼ�
      begin
        FMouseInControl := True;
      end;
    CM_MOUSELEAVE:  // ����뿪�ؼ�
      begin
        FMouseInControl := False;
      end;
  end;

  FSkinWindowProc(Message);   // ִ�� Skin WindowProc ����
end;

{ TCnSkinMagic }

{------------------------------------------------------------------------------}
// RegisterClass - ע�� SkinMagic Class, ���ע��ʧ��, �������쳣
//
// ����: AClass    �� SubClass ����, ����Ϊ TControlClass �������
//       AWndProc  SubClass ��� WindowProc, ���û����
{------------------------------------------------------------------------------}
class procedure TCnSkinMagic.RegisterClass(AClass: TControlClass;
  AWindowProc: Pointer);
var
  ConstructionPtr, DestructionPtr: Pointer;
  OldConstruction, OldDestruction: Pointer;
  ClassDataPtr: PClassData;
begin
  // ȡԭʼ AfterConstruction vmt ָ��
  ConstructionPtr := Pointer(TCnNativePointer(AClass) + vmtAfterConstruction);
  // ȡԭʼ AfterConstruction ��ַ
  OldConstruction := Pointer(TCnNativeIntPtr(ConstructionPtr)^);
  // ��д vmt ָ��
  WriteVmtPtr(ConstructionPtr, @CnAfterConstruction);

  // ȡԭʼ BeforeDestruction vmt ָ��
  DestructionPtr := Pointer(TCnNativePointer(AClass) + vmtBeforeDestruction);
  // ȡԭʼ BeforeDestruction ��ַ
  OldDestruction := Pointer(TCnNativeIntPtr(DestructionPtr)^);
  // ��д vmt ָ��
  WriteVmtPtr(DestructionPtr, @CnBeforeDestruction);

  // ��������Ϣ��ȫ�� List
  New(ClassDataPtr);
  ClassDataPtr^.ClassType := AClass;
  ClassDataPtr^.WindowProc := AWindowProc;
  ClassDataPtr^.OldAfterConstruction := OldConstruction;
  ClassDataPtr^.OldBeforeDestruction := OldDestruction;
  FClassList.Add(ClassDataPtr);
end;

class procedure TCnSkinMagic.RefreshControls;
var
  I, J: Integer;
  Form: TForm;          // ʹ����ʱ�������������Է��ʵĿ���
  Control: TControl;
begin
  if not Assigned(Screen) then
    Exit;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if not Form.Visible then
      Continue;

    for J := 0 to Form.ControlCount - 1 do
    begin
      Control := Form.Controls[J];
      if not Control.Visible then
        Continue;

      Control.Visible := False;
      Control.Visible := True;
    end;
  end;
end;

class procedure TCnSkinMagic.EnableSkin;
begin
  if not FSkinActive then
  begin
    FSkinActive := True;
    RefreshControls;
  end;
end;

class procedure TCnSkinMagic.DisableSkin;
begin
  if FSkinActive then
  begin
    FSkinActive := False;
    RefreshControls;
  end;
end;

function TCnSkinMagic.GetSkinActive: Boolean;
begin
  Result := FSkinActive;
end;

procedure TCnSkinMagic.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnSkinMagicName;
  Author := SCnPack_Savetime;
  Email := SCnPack_SavetimeEmail;
  Comment := SCnSkinMagicComment;
end;

initialization
  // ����ȫ�� TCnClassList ����, ���ڹ������ TClassData ��Ϣ
  FClassList := TCnClassList.Create;

  // ע��ȫ����Ϣ, ����֪ͨ���������ʱ TControlSubClass ͬʱ������
  CN_MSG_BEFORE_DESTRUCTION := RegisterWindowMessage('CnSkinMagic_BeforeDestruction');
  if CN_MSG_BEFORE_DESTRUCTION = 0 then
    raise Exception.Create(SCNE_REGISTERMESSAGEFAILED);

finalization
  // �ͷ� FClassList ����
  FClassList.Free;

end.
