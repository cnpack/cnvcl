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

unit CnCallBack;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：回调转换的工具单元
* 单元作者：CnPack开发组 savetime (savetime2k@yahoo.com)
*           刘啸 (liuxiao@cnpack.org)
* 备    注：该单元是回调转换等的代码单元
*           包装的代码部分在自行分配的可执行的内存空间，避免了 DEP 下出错。
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2006.10.13 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils;

type
  ECallBackException = class(Exception)
  end;

function StdcallMethodToCallBack(ASelf: Pointer; AMethodAddr: Pointer): Pointer;
{* 将 stdcall 的类成员函数和实例加以包装，返回一个新的 stdcall 的回调函数地址 }

{* 使用语法:
  @AStdCallbackFunc := StdcallMethodToCallBack(AObject, @TAObject.CallbackMethod);
  其中 AStdCallbackFunc 和 CallbackMethod 都必须使用 stdcall 声明。
}

implementation

type
  TCnCallback = array [1..18] of Byte; // 按代码中最长的来
  PCnCallback = ^TCnCallback;
  
const
  THUNK_SIZE = 4096; // x86 页大小，目前只弄一个页面

  StdcallCode: TCnCallback =
    ($8B,$04,$24,$50,$B8,$00,$00,$00,$00,$89,$44,$24,$04,$E9,$00,$00,$00,$00);

  {----------------------------}
  { Stdcall CallbackCode ASM   }
  {----------------------------}
  {    MOV EAX, [ESP];         }
  {    PUSH EAX;               }
  {    MOV EAX, ASelf;         }
  {    MOV [ESP+4], EAX;       }
  {    JMP AMethodAddr;        }
  {----------------------------}

var
  FCallBackPool: Pointer = nil;
  FEmptyPtr: Integer = 0;
  FCS: TRTLCriticalSection;

procedure InitCallBackPool;
begin
  FCallBackPool := VirtualAlloc(nil, THUNK_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if FCallBackPool = nil then
    raise ECallBackException.Create('Callback Pool Init Error!');
end;

function StdcallMethodToCallBack(ASelf: Pointer; AMethodAddr: Pointer): Pointer;
var
  Instance: PCnCallback;
begin
  Result := nil;
  Instance := nil;
  
  try
    EnterCriticalSection(FCS);

    if FCallBackPool = nil then
    begin
      InitCallBackPool;
      Instance := FCallBackPool;
    end
    else
    begin
      if FEmptyPtr = (THUNK_SIZE div SizeOf(TCnCallback)) then
        raise ECallBackException.Create('Callback Pool Overflow!');

      Inc(FEmptyPtr);
      Instance := PCnCallback(Integer(FCallBackPool) + FEmptyPtr * SizeOf(TCnCallback));
    end;
  finally
    LeaveCriticalSection(FCS);
  end;

  if Instance <> nil then
  begin
    Move(StdcallCode, Instance^, SizeOf(TCnCallback));
    PInteger(@(Instance^[6]))^ := Integer(ASelf);
    PInteger(@(Instance^[15]))^ := Integer(Integer(AMethodAddr) - Integer(Instance) - 18);
    Result := Instance;
  end;
end;

initialization
  InitializeCriticalSection(FCS);

finalization
  DeleteCriticalSection(FCS);
  if FCallBackPool <> nil then
    VirtualFree(FCallBackPool, 0, MEM_RELEASE);

end.
