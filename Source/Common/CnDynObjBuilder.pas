{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnDynObjBuilder;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：通过字符串传递，动态的建立对象
* 单元作者：Eric Wang Email: Eric@SimpleDataAccess.net
* 备    注：
* 开发平台：
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2010.02.04 V1.1
*               移植入 CnVCL
*           2008.03.17 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

uses Classes, TypInfo;

type
  TCollectionClass = class of TCollection;

  //动态对象构建器
  TCnDynamicBuilder = class
  public
    class function BuildCollection(ClassName: string; CollectionItemClass:
      TCollectionItemClass): TCollection;
    class function BuildCollectionItem(ClassName: string; Collection:
      TCollection): TCollectionItem;
    class function BuildComponent(const ClassName: string; const AOwner:
      TComponent = nil): TComponent;
    class function BuildPersistent(ClassName: string): TPersistent;
  end;

implementation

{ TCnDynamicBuilder }

class function TCnDynamicBuilder.BuildCollection(ClassName: string;
  CollectionItemClass: TCollectionItemClass): TCollection;
begin
  Result := TCollectionClass(FindClass(ClassName)).Create(CollectionItemClass);
end;

class function TCnDynamicBuilder.BuildCollectionItem(ClassName: string;
  Collection: TCollection): TCollectionItem;
begin
  Result := TCollectionItemClass(FindClass(ClassName)).Create(Collection);
end;

class function TCnDynamicBuilder.BuildComponent(const ClassName: string; const
  AOwner: TComponent = nil): TComponent;
begin
  Result := TComponentClass(FindClass(ClassName)).Create(AOwner);
end;

class function TCnDynamicBuilder.BuildPersistent(ClassName: string): TPersistent;
begin
  Result := TPersistentClass(FindClass(ClassName)).Create;
end;

end.
