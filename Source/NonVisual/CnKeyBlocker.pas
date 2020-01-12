{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnKeyBlocker;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：通过键盘钩子屏蔽系统键的组件
* 单元作者：匿名
* 移    植：刘啸 (liuxiao@cnpack.org)
* 备    注：此组件通过实现键盘钩子来屏蔽某些系统键，但 Ctrl+Alt+Del 组合键可能
*           因为系统自身处理原因而无法屏蔽。
* 开发平台：PWinXP + Delphi 7.0 (Build 8.1)
* 兼容测试：PWin2003 + Delphi 7.0 (Build 8.1)
* 本 地 化：该单元中无字符串资源
* 修改记录：2008.10.24 v1.1
*               加入一简陋事件
*           2008.05.29 v1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, windows, ShlObj, Registry, Shellapi, Messages;

type
  TCnBlockKeyEvent = procedure(VirtualKey: Cardinal) of object;

  TCnKeyBlocker = class(TComponent)
  private
    FBCAD: Boolean;
    FBAT: Boolean;
    FBCE: Boolean;
    FEnabled: Boolean;
    FBAE: Boolean;
    FBCR: Boolean;
    FBCK: Boolean;
    FBP: Boolean;
    FBS: Boolean;
    FCKC: Cardinal;
    FBWA: Boolean;
    FBCAE: Boolean;
    FOnBlockKey: TCnBlockKeyEvent;
    procedure SetBCAD(const Value: Boolean);
    procedure SetBAT(const Value: Boolean);
    procedure SetBCE(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetBAE(const Value: Boolean);
    procedure SetBCK(const Value: Boolean);
    procedure SetBCR(const Value: Boolean);
    procedure SetBP(const Value: Boolean);
    procedure SetBS(const Value: Boolean);
    procedure SetBWA(const Value: Boolean);
    procedure SetBCAE(const Value: Boolean);
  protected
    procedure UpdateKeyBlock;
    procedure DoBlock(VirtualKey: Cardinal);
    property BlockCtrlAltDelete: Boolean read FBCAD write SetBCAD;
    {* 是否屏蔽 Ctrl+Alt+Delete 键，此属性可能无法工作，暂时不开放}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BlockAltTab: Boolean read FBAT write SetBAT;
    {* 是否屏蔽 Alt+Tab 键}
    property BlockCtrlEsc: Boolean read FBCE write SetBCE;
    {* 是否屏蔽 Ctrl+Esc 键}
    property BlockAltEsc: Boolean read FBAE write SetBAE;
    {* 是否屏蔽 Alt+Esc 键}
    property BlockCtrlEnter: Boolean read FBCR write SetBCR;
    {* 是否屏蔽 Ctrl+Enter 键}
    property BlockSleep: Boolean read FBS write SetBS;
    {* 是否屏蔽 Sleep 休眠键}
    property BlockPower: Boolean read FBP write SetBP;
    {* 是否屏蔽 Power 电源键}
    property BlockWinApps: Boolean read FBWA write SetBWA;
    {* 是否屏蔽 Windows 键}
    property BlockCtrlAltEnter: Boolean read FBCAE write SetBCAE;
    {* 是否屏蔽 Ctrl+Alt+Enter 键}

    property CustomKeyCode: Cardinal read FCKC write FCKC default 0;
    {* 自定义的屏蔽键}
    property BlockCustomKey: Boolean read FBCK write SetBCK;
    {* 是否屏蔽自定义键}

    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* 是否使能屏蔽功能}

    property OnBlockKey: TCnBlockKeyEvent read FOnBlockKey write FOnBlockKey;
    {* 屏蔽键时触发的事件，由于复杂性，参数中只指明虚拟键，并且由于挂接机制本身
       的机制，此事件无 Sender。}
  end;

implementation

const
  LLKHF_ALTDOWN = KF_ALTDOWN shr 8;
  WH_KEYBOARD_LL = 13;

type
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;
  KBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    Time: DWORD;
    dwExtraInfo: DWORD;
  end;

var
  hhkNTKeyboard: HHOOK = 0;
  aBCAD: Boolean = False;
  aBWA: Boolean = False;
  aBCE: Boolean = False;
  aBAT: Boolean = False;
  aBAE: Boolean = False;
  aBCR: Boolean = False;
  aBCAE: Boolean = False;
  aBP: Boolean = False;
  aaBS: Boolean = False;
  aBCK: Boolean = False;
  aCKC: Cardinal = 0;

  FKeyBlocker: TCnKeyBlocker = nil;

{ TCnKeyBlocker }

constructor TCnKeyBlocker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyBlocker := Self;
end;

procedure EnableCTRLALTDEL(YesNo: Boolean);
const
  sRegPolicies = '\Software\Microsoft\Windows\CurrentVersion\Policies';
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(sRegPolicies + '\System\', True) then
    begin
      case YesNo of
        False:
          begin
            WriteInteger('DisableTaskMgr', 1); //任务管理
            WriteInteger('DisableLockWorkstation', 1); //用户锁定计算机
            WriteInteger('DisableChangePassword', 1); //用户更改口令
          end;
        True:
          begin
            WriteInteger('DisableTaskMgr', 0);
            WriteInteger('DisableLockWorkstation', 0);
            WriteInteger('DisableChangePassword', 0);
          end;
      end;
    end;
    CloseKey;
    if OpenKey(sRegPolicies + '\Explorer\', True) then
    begin
      case YesNo of
        False:
          begin
            WriteInteger('NoChangeStartMenu', 1); //开始菜单
            WriteInteger('NoClose', 1); // 关闭系统菜单
            WriteInteger('NoLogOff', 1); //注销菜单
            WriteInteger('NoRun', 1); //运行菜单
            WriteInteger('NoSetFolders', 1); //设置菜单
          end;
        True:
          begin
            WriteInteger('NoChangeStartMenu', 0);
            WriteInteger('NoClose', 0);
            WriteInteger('NoLogOff', 0);
            WriteInteger('NoRun', 0);
          end;
      end;
    end;
    CloseKey;
  finally
    Free;
  end;
end;

function LowLevelKeyboardFunc(nCode: INTEGER; w_Param: WPARAM;
  l_Param: LPARAM): LRESULT; stdcall;
var
  boolKey: Boolean;
  p: PKBDLLHOOKSTRUCT;
const
  VK_SLEEP = $5F;
  VK_POWER = $5E;
begin
  boolKey := False;
  p := nil;
  if nCode = HC_ACTION then
  begin
    case w_Param of
      WM_KEYDOWN, WM_SYSKEYDOWN, WM_KEYUP, WM_SYSKEYUP:
        begin
          p := PKBDLLHOOKSTRUCT(l_Param);
      //---------!-~------------------------------------------------
      {    if ((GetAsyncKeyState(VK_RBUTTON) and $8000) <> 0) then boolKey := True;
          if (CHAR(p.vkCode) >= '!') and (CHAR(p.vkCode) <= '~') and
            ((GetKeyState(VK_CONTROL) and $8000) <> 0) then boolKey := True;
          if (p.vkCode = VK_SPACE) and
            ((GetKeyState(VK_CONTROL) and $8000) <> 0) then boolKey := True;    }

      //---------F1-F12 ----------------------------------------------
      {    if (p.vkCode = VK_F1) or (p.vkCode = VK_F2) or (p.vkCode = VK_F3) or
            (p.vkCode = VK_F4) or (p.vkCode = VK_F5) or (p.vkCode = VK_F6) or
            (p.vkCode = VK_F7) or (p.vkCode = VK_F8) or (p.vkCode = VK_F9) or
            (p.vkCode = VK_F10) or (p.vkCode = VK_F11) or (p.vkCode = VK_F12) then
            boolKey := True;

          if ((p.vkCode = VK_F1) or (p.vkCode = VK_F2) or (p.vkCode = VK_F3) or
            (p.vkCode = VK_F4) or (p.vkCode = VK_F5) or (p.vkCode = VK_F6) or
            (p.vkCode = VK_F7) or (p.vkCode = VK_F8) or (p.vkCode = VK_F9) or
            (p.vkCode = VK_F10) or (p.vkCode = VK_F11) or (p.vkCode = VK_F12)) and
            (((GetKeyState(VK_MENU) and $8000) <> 0) or ((GetKeyState(VK_CONTROL) and $8000) <> 0)
             or ((GetKeyState(VK_SHIFT)and$8000) <> 0)) then
              boolKey := True; }

      //-------系统热键---------------------------------------------
      //WIN(Left or Right)+APPS
          if aBWA then
          begin
            if (p.vkCode = VK_LWIN) or (p.vkCode = VK_RWIN) or (p.vkCode = VK_APPS) then
              boolKey := True;
          end;
      //CTRL+ESC
          if aBCE then
          begin
            if (p.vkCode = VK_ESCAPE) and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //ALT+TAB
          if aBAT then
          begin
            if (p.vkCode = VK_TAB) and ((GetAsyncKeyState(VK_MENU) and $8000) <> 0) then
              boolKey := True;
          end;
      //ALT+ESC
          if aBAE then
          begin
            if (p.vkCode = VK_ESCAPE) and ((p.flags and LLKHF_ALTDOWN) <> 0) then
              boolKey := True;
          end;
      //CTRL+ENTER
          if aBCR then
          begin
            if (p.vkCode = VK_RETURN) and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //CTRL+ALT+ENTR
          if aBCAE then
          begin
            if (p.vkCode = VK_RETURN) and ((p.flags and LLKHF_ALTDOWN) <> 0)
              and ((GetKeyState(VK_CONTROL) and $8000) <> 0) then
              boolKey := True;
          end;
      //POWER
          if aBP then
          begin
            if (p.vkCode = VK_POWER) then
              boolKey := True;
          end;
      //SLEEP
          if aaBS then
          begin
            if (p.vkCode = VK_SLEEP) then
              boolKey := True;
          end;
      //Custom
          if aBCK then
          begin
            if (p.vkCode = aCKC) then
              boolKey := True;
          end;

      //如果有其他要屏闭的键，添加在此处
        end;
    end;
  end;

  //捕获这些组合键，按键消息由自己处理，必须返回 1
  if boolKey and (p <> nil) then
  begin
    FKeyBlocker.DoBlock(p.vkCode);
    Result := 1;
    Exit;
  end;

  //其他的按键，交由别的线程处理（过滤）
  Result := CallNextHookEx(0, nCode, w_Param, l_Param);
end;

destructor TCnKeyBlocker.Destroy;
begin
  Enabled := False;
  FKeyBlocker := nil;
  inherited Destroy;
end;

procedure TCnKeyBlocker.DoBlock(VirtualKey: Cardinal);
begin
  if Assigned(FOnBlockKey) then
    FOnBlockKey(VirtualKey);
end;

procedure TCnKeyBlocker.SetBAE(const Value: Boolean);
begin
  FBAE := Value;
  aBAE := FBAE;
end;

procedure TCnKeyBlocker.SetBAT(const Value: Boolean);
begin
  FBAT := Value;
  aBAT := FBAT;
end;

procedure TCnKeyBlocker.SetBCAD(const Value: Boolean);
begin
  FBCAD := Value;
  aBCAD := FBCAD;
end;

procedure TCnKeyBlocker.SetBCAE(const Value: Boolean);
begin
  FBCAE := Value;
  aBCAE := FBCAE;
end;

procedure TCnKeyBlocker.SetBCE(const Value: Boolean);
begin
  FBCE := Value;
  aBCE := FBCE;
end;

procedure TCnKeyBlocker.SetBCK(const Value: Boolean);
begin
  FBCK := Value;
  aBCK := FBCK;
end;

procedure TCnKeyBlocker.SetBCR(const Value: Boolean);
begin
  FBCR := Value;
  aBCR := FBCR;
end;

procedure TCnKeyBlocker.SetBP(const Value: Boolean);
begin
  FBP := Value;
  aBP := FBP;
end;

procedure TCnKeyBlocker.SetBS(const Value: Boolean);
begin
  FBS := Value;
  aaBS := FBS;
end;

procedure TCnKeyBlocker.SetBWA(const Value: Boolean);
begin
  FBWA := Value;
  aBWA := FBWA;
end;

procedure TCnKeyBlocker.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateKeyBlock;
end;

procedure TCnKeyBlocker.UpdateKeyBlock;
begin
  if csDesigning in ComponentState then
    Exit;

  case FEnabled of
    True:
      begin
        if hhkNTKeyboard <> 0 then
          Exit;

        hhkNTKeyboard := SetWindowsHookEx(WH_KEYBOARD_LL, LowLevelKeyboardFunc, HInstance, 0);
        if FBCAD then
        begin
          EnableCTRLALTDEL(False);
          SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
        end;
      end;
    False:
      begin
        if hhkNTKeyboard = 0 then
          Exit;
        UnhookWindowsHookEx(hhkNTKeyboard); // 卸载钩子
        EnableCTRLALTDEL(True);
        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
        hhkNTKeyboard := 0;
      end;
  end;
end;

end.
