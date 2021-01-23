{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnCompUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：组件工具单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 开发平台：PWinXP SP2 + Delphi 5.01
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 备    注：
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls;

procedure AddComponentFreeNotify(var AComponent: TComponent; OnFree: TNotifyEvent);
{* 增加一个组件释放通知。如果 OnFree 为 nil，组件释放时会将 AComponent 变量设为 nil }
procedure RemoveComponentFreeNotify(var AComponent: TComponent);
{* 删除一个组件释放通知。}

implementation

type
  PCnCompFreeNotifyRec = ^TCnCompFreeNotifyRec;
  TCnCompFreeNotifyRec = record
    Comp: TComponent;
    PComp: ^TComponent;
    OnFree: TNotifyEvent;
  end;
  
  TCnCompFreeNotifyObj = class(TComponent)
  private
    FList: TList;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(var AComponent: TComponent; OnFree: TNotifyEvent): Integer;
    procedure Remove(var AComponent: TComponent);
  end;

{ TCnCompFreeNotifyObj }

function TCnCompFreeNotifyObj.Add(var AComponent: TComponent;
  OnFree: TNotifyEvent): Integer;
var
  P: PCnCompFreeNotifyRec;
begin
  Result := -1;
  if AComponent <> nil then
  begin
    New(P);
    P^.Comp := AComponent;
    P^.PComp := @AComponent;
    P^.OnFree := OnFree;
    Result := FList.Add(P);
    AComponent.FreeNotification(Self);
  end;
end;

constructor TCnCompFreeNotifyObj.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

destructor TCnCompFreeNotifyObj.Destroy;
var
  P: PCnCompFreeNotifyRec;
begin
  while FList.Count > 0 do
  begin
    P := FList.Extract(FList[0]);
    Dispose(P);
  end;
  FList.Free;  
  inherited;
end;

procedure TCnCompFreeNotifyObj.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
  P: PCnCompFreeNotifyRec;
begin
  inherited;
  if Operation = opRemove then
  begin
    for i := FList.Count - 1 downto 0 do
    begin
      P := FList[i];
      if P^.Comp = AComponent then
      begin
        P^.PComp^ := nil;
        if Assigned(P^.OnFree) then
          P^.OnFree(AComponent);
        Dispose(P);
        FList.Delete(i);
      end;  
    end;
  end;    
end;

procedure TCnCompFreeNotifyObj.Remove(var AComponent: TComponent);
var
  i: Integer;
  P: PCnCompFreeNotifyRec;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    P := FList[i];
    if P^.PComp = @AComponent then
    begin
      Dispose(P);
      FList.Delete(i);
    end;
  end;
end;

var
  FCnCompFreeNotifyObj: TCnCompFreeNotifyObj;

// 增加一个组件释放通知。如果 OnFree 为 nil，组件释放时会将 AComponent 变量设为 nil
procedure AddComponentFreeNotify(var AComponent: TComponent; OnFree: TNotifyEvent);
begin
  if FCnCompFreeNotifyObj = nil then
    FCnCompFreeNotifyObj := TCnCompFreeNotifyObj.Create(nil);
  FCnCompFreeNotifyObj.Add(AComponent, OnFree);
end;

// 删除一个组件释放通知。
procedure RemoveComponentFreeNotify(var AComponent: TComponent);
begin
  if FCnCompFreeNotifyObj <> nil then
    FCnCompFreeNotifyObj.Remove(AComponent);
end;

initialization

finalization
  if FCnCompFreeNotifyObj <> nil then
    FreeAndNil(FCnCompFreeNotifyObj);

end.
