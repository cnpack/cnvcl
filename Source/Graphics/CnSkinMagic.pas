{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnSkinMagic;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包单元
* 单元名称：运行期换皮肤框架，皮肤效果需自实现
* 单元作者：CnPack开发组 savetime
            (savetime2k@hotmail.com, http://savetime.delphibbs.com)
* 备    注：本单元由原作者授权 CnPack 开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2007.07.27 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  CnNativeDecl, CnClasses, CnConsts, CnCompConsts, CnGraphConsts;

type

  TControlSubClass = class(TObject)
  {* 控件被 SubClass 之后的依附的对象，Skin 设计者需要了解 public 域的属性}
  private
    FControl: TControl;
    FSkinWindowProc: TWndMethod;
    FOldWindowProc: TWndMethod;
    FIsWinControl: Boolean;
    FMouseInControl: Boolean;
    constructor Create(AControl: TControl);
    {* Skin 设计者无需调用这个,所以将它藏在 private 里}
    procedure WindowProc(var Message: TMessage);
  public
    property Control: TControl read FControl;
    {* 被 SubClass 后的控件(只读) }
    property OldWindowProc: TWndMethod read FOldWindowProc;
    {* 控件原始的 WindowProc }
    property IsWinControl: Boolean read FIsWinControl;
    {* 当前控件是否是 TWinControl 类型 }
    property MouseInControl: Boolean read FMouseInControl;
    {* 鼠标是否在当前控件中 }    
  end;

  TCnSkinMagic = class(TCnComponent)
  {* 运行期换皮肤组件，可不实例化而直接使用其类方法 }
  private
    function GetSkinActive: Boolean;
  protected
    class procedure RefreshControls;
    {* 重新刷新所有控件}
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    class procedure EnableSkin;
    {* 使能 CnSkinMagic 的窗口过程挂接 }
    class procedure DisableSkin;
    {* 禁用 CnSkinMagic 的窗口过程挂接 }
    class procedure RegisterClass(AClass: TControlClass; AWindowProc: Pointer);
    {* 注册需要挂接的 CnSkinMagic Class, 如果注册失败, 将触发异常
       参数: AClass    需 SubClass 的类, 必须为 TControlClass 及子类
             AWndProc  SubClass 后的 WindowProc, 由用户设计
       注意：AWindowProc 必须具有以下的格式：
       procedure AWindowProc(Self: TControlSubClass; var Message: TMessage);
    }
  published
    property SkinActive: Boolean read GetSkinActive;
    {* 返回当前 Skin 是否激活, 由 EnableSkin 和 DisableSkin 控制}
  end;

{==============================================================================}
{ IMPLEMENTATION - 以下是具体实现内容, Skin 设计者可以不看以下代码             }
{==============================================================================}

implementation

type
  TAfterConstruction = procedure(Self: Pointer);
  TBeforeDestruction = procedure(Self: Pointer);
  // TObject.AfterConstruction & BeforeDestruction 函数类型

  TCnClassList = class(TList)
  {* 用于管理用户定义的 ClassData }
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  PClassData = ^TClassData;
  TClassData = record
  {* TCnClassList 中的数据项 }
    ClassType: TClass;
    WindowProc: Pointer;
    OldAfterConstruction: Pointer;
    OldBeforeDestruction: Pointer;
  end;

var
  FClassList: TCnClassList;
  {* (全局)用户定义的类信息(TClassData)列表 }
  FSkinActive: Boolean;
  {* 当前 Skin 是否激活, 由 EnableSkin 和 DisableSkin  控制 }
  CN_MSG_BEFORE_DESTRUCTION: Cardinal;
  {* CN_MSG_BEFORE_DESTRUCTION 消息, 通知 TControlSubClass 自行析构}

{==============================================================================}
{ 以下是辅助函数                                                               }
{==============================================================================}

{------------------------------------------------------------------------------}
// FindClassData - 在 FClassList 中找到控件的 TClassData 信息
{------------------------------------------------------------------------------}
function FindClassData(AClass: TClass): PClassData;
var
  I: Integer;
begin
  for I := 0 to FClassList.Count - 1 do
  begin
    Result := PClassData(FClassList.Items[I]);
    if AClass = Result.ClassType then Exit;
  end;
  raise Exception.Create(SCNE_FINDCLASSDATAFAILED);
end;

{------------------------------------------------------------------------------}
// MakeMethod - 将一般静态函数转换为 TMethod 类型
{------------------------------------------------------------------------------}
function MakeMethod(Self: Pointer; FuncAddr: Pointer): TMethod;
begin
  Result.Code := FuncAddr;
  Result.Data := Self;
end;

{------------------------------------------------------------------------------}
// CnAfterConstruction - 被替换的 AfterConstruction 函数, 将在控件被构造后调用
// 此函数被调用后创建 TControlSubClass 对象并与新构造的控件捆绑在一起
// 最后还调用了原始的 AfterConstruction 函数
{------------------------------------------------------------------------------}
procedure CnAfterConstruction(Self: TControl);
var
  OldAfterConstruction: TAfterConstruction;
  ClassDataPtr: PClassData;
begin
  // 新建一个 ControlSubClass 对象, 用于控制当前控件的行为
  TControlSubClass.Create(Self);

  // 找到当前控件的(用户定义的)类数据
  ClassDataPtr := FindClassData(Self.ClassType);
  // 查找(用户定义的)类数据中初始的 AfterConstruction 方法
  OldAfterConstruction := ClassDataPtr^.OldAfterConstruction;
  // 如果有 AfterConstruction 方法, 则执行它
  if Assigned(OldAfterConstruction) then OldAfterConstruction(Self);
end;

{------------------------------------------------------------------------------}
// CnBeforeDestruction - 被替换的 BeforeDestruction 函数, 将在控件被析构前调用
// 此函数被调用后发送消息给 TControlSubClass 对象, 使之同时析构
// 最后还调用了原始的 BeforeDestruction 函数
{------------------------------------------------------------------------------}
procedure CnBeforeDestruction(Self: TControl);
var
  OldBeforeDestruction: TBeforeDestruction;
  ClassDataPtr: PClassData;
begin
  // 发送消息通知 TControlSubClass 析构
  Self.Perform(CN_MSG_BEFORE_DESTRUCTION, 0, 0);

  // 找到当前控件的(用户定义的)类数据
  ClassDataPtr := FindClassData(Self.ClassType);
  // 查找(用户定义的)类数据中初始的 BeforeDestruction 方法
  OldBeforeDestruction := ClassDataPtr^.OldBeforeDestruction;
  // 如果有 BeforeDestruction 方法, 则执行它
  if Assigned(OldBeforeDestruction) then
    OldBeforeDestruction(Self);
end;

{------------------------------------------------------------------------------}
// WriteVmtPtr - 改写 vmt 指针值 (写代码段)
{------------------------------------------------------------------------------}
procedure WriteVmtPtr(VmtPtrAddr: Pointer; AFuncAddr: Pointer);
var
  ProcessHandle: THandle;
  WriteBytesCount: TCnNativeUInt;
begin
  ProcessHandle := OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_WRITE,
    False, GetCurrentProcessId());
  {TODO: 是不是要检查一下 ProcessHandle }

  WriteProcessMemory(ProcessHandle, VmtPtrAddr, @AFuncAddr, 4, WriteBytesCount);

  CloseHandle(ProcessHandle);

  if WriteBytesCount <> 4 then
    raise Exception.Create(SCNE_WRITEVMTFAILED);
end;

{------------------------------------------------------------------------------}
// TCnClassList.Notify - 在 Item 被删除时自动回收 TClassData 的内容
{------------------------------------------------------------------------------}
procedure TCnClassList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    Dispose(Ptr);
end;

{------------------------------------------------------------------------------}
// TControlSubClass.Create - 创建 SubClass 对象并与控件捆绑在一起
{------------------------------------------------------------------------------}
constructor TControlSubClass.Create(AControl: TControl);
var
  ClassDataPtr: PClassData;
begin
  // 找到当前控件的(用户定义的)类数据
  ClassDataPtr := FindClassData(AControl.ClassType);
  // 在 ControlSubClass 中保存当前控件指针
  FControl := AControl;
  // 在 ControlSubClass 中存储当前控件是否是 TWinControl
  FIsWinControl := AControl is TWinControl;
  // 合成 FSkinWindowProc;
  FSkinWindowProc := TWndMethod(MakeMethod(Self, ClassDataPtr^.WindowProc));
  // 在 ControlSubClass 中保存当前控件的窗口过程
  FOldWindowProc := AControl.WindowProc;
  // 设置当前对象的窗口过程为(用户定义的)类数据中的窗口过程
  AControl.WindowProc := Self.WindowProc;
end;

{------------------------------------------------------------------------------}
// TControlSubClass.WindowProc - 被 SubClass 时第一个被调用的 WindowProc
{------------------------------------------------------------------------------}
procedure TControlSubClass.WindowProc(var Message: TMessage);
begin
  if Message.Msg = CN_MSG_BEFORE_DESTRUCTION then  // 控件被析构
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
    CM_MOUSEENTER:  // 鼠标进入控件
      begin
        FMouseInControl := True;
      end;
    CM_MOUSELEAVE:  // 鼠标离开控件
      begin
        FMouseInControl := False;
      end;
  end;

  FSkinWindowProc(Message);   // 执行 Skin WindowProc 过程
end;

{ TCnSkinMagic }

{------------------------------------------------------------------------------}
// RegisterClass - 注册 SkinMagic Class, 如果注册失败, 将触发异常
//
// 参数: AClass    需 SubClass 的类, 必须为 TControlClass 及后继类
//       AWndProc  SubClass 后的 WindowProc, 由用户设计
{------------------------------------------------------------------------------}
class procedure TCnSkinMagic.RegisterClass(AClass: TControlClass;
  AWindowProc: Pointer);
var
  ConstructionPtr, DestructionPtr: Pointer;
  OldConstruction, OldDestruction: Pointer;
  ClassDataPtr: PClassData;
begin
  // 取原始 AfterConstruction vmt 指针
  ConstructionPtr := Pointer(Integer(AClass) + vmtAfterConstruction);
  // 取原始 AfterConstruction 地址
  OldConstruction := Pointer(PInteger(ConstructionPtr)^);
  // 改写 vmt 指针
  WriteVmtPtr(ConstructionPtr, @CnAfterConstruction);

  // 取原始 BeforeDestruction vmt 指针
  DestructionPtr := Pointer(Integer(AClass) + vmtBeforeDestruction);
  // 取原始 BeforeDestruction 地址
  OldDestruction := Pointer(PInteger(DestructionPtr)^);
  // 改写 vmt 指针
  WriteVmtPtr(DestructionPtr, @CnBeforeDestruction);

  // 保存类信息至全局 List
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
  Form: TForm;          // 使用临时变量减少类属性访问的开销
  Control: TControl;
begin
  if not Assigned(Screen) then Exit;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if not Form.Visible then Continue;
    for J := 0 to Form.ControlCount - 1 do
    begin
      Control := Form.Controls[J];
      if not Control.Visible then Continue;
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
  // 创建全局 TCnClassList 对象, 用于管理类的 TClassData 信息
  FClassList := TCnClassList.Create;

  // 注册全局消息, 用于通知组件被析构时 TControlSubClass 同时被析构
  CN_MSG_BEFORE_DESTRUCTION := RegisterWindowMessage('CnSkinMagic_BeforeDestruction');
  if CN_MSG_BEFORE_DESTRUCTION = 0 then
    raise Exception.Create(SCNE_REGISTERMESSAGEFAILED);

finalization
  // 释放 FClassList 对象
  FClassList.Free;

end.
