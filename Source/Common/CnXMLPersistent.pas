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

unit CnXMLPersistent;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：XML Serialize Unit
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

{$I CnPack.inc}

uses
  Classes, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}
  xmldom, XMLIntf, msxmldom, XMLDoc, TypInfo, Graphics;

const
  ROOT_OBJECT = 'XMLPersistent';
  tkPersistent = [tkInteger, tkChar, tkEnumeration, tkSet,
    tkClass, tkInterface, tkFloat, tkWChar, tkString, tkLString, tkWString
    {$IFDEF UNICODE_STRING}, tkUString{$ENDIF}, tkVariant, tkInt64,
    tkRecord, tkArray, tkDynArray, tkUnknown];
  tkObj = [tkClass, tkInterface];
  tkStr = [tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}];
  tkValue = tkPersistent - tkObj;
  tkOthers = [tkMethod];
  tkAll = tkPersistent + tkOthers;
  IID_IStreamPersist: TGUID = '{B8CD12A3-267A-11D4-83DA-00C04F60B2DD}';

type
  // XML Serialize base class
  TCnXMLObjectFiler = class(TComponent)
  private
    FEncoding: string;
  protected
    FRootNode: IXMLNode;
    FXMLDoc: TXmlDocument;
    function FindNode(const NodeName: string): IXMLNode;
    function GetXMLString: string;
    procedure InitXMLHead;
    function NodeExists(const NodeName: string): Boolean;
    procedure SetXMLString(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; virtual;
  published
    property Encoding: string read FEncoding write FEncoding;
    property XMLString: string read GetXMLString write SetXMLString;
  end;

  // Write a object to XML  (object must inherited tpersistent class)
  TCnXMLObjectWriter = class(TCnXMLObjectFiler)
  private
    procedure WriteProperties(const Obj: TPersistent; Prop: PPropInfo; Node:
      IXMLNode);
    procedure SavelCollectionItem(const Obj: TPersistent; const Node: IXMLNode);
    procedure SetClassType(const Obj: TPersistent; Nde: IXMLNode);
    procedure SetPersistentType(const Obj: TPersistent; const Node: IXMLNode);
    procedure WriteXMLData(const Obj: TPersistent; const Nde: IXMLNode);
  protected
    procedure WriteObject(Prop: PPropInfo; Node: IXMLNode; Obj: TObject); virtual;
    procedure WritePersistentObject(const Obj: TPersistent; Node: IXMLNode);
      virtual;
    procedure WriteValue(ParentNode: IXMLNode; const Name: string; const Value:
      Variant);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromXMLString(XML: wideString);
    procedure SaveXMLFile(XMLFileName: string);
    procedure WriteObjectToXML(ObjectName: string; const Obj: TPersistent);
    procedure WriteValueToXML(Name: string; const Value: Variant);
  end;

  // Read a xml to Object
  TCnXMLObjectReader = class(TCnXMLObjectFiler)
  private
    procedure FindRootNode;
    function PropIsReadOnly(Pinfo: PPropInfo): Boolean;
    function PropIsValue(Pinfo: PPropInfo): Boolean;
    procedure ReadCollection(Collection: TCollection; Node: IXMLNode);
    procedure ReadPersistentFromXML(Node: IXMLNode; Instance: TPersistent);
    procedure BuildInterfaceFromXML(Node: IXMLNode; Instance: IInterface);
    procedure SetObjPropValue(PropName: string; Instance, PropObj: TPersistent);
    procedure ReadXMLObject(Instance: TPersistent; PropNode: IXMLNode; PProp:
      PPropInfo);
    procedure SetXMLPropValue(Obj: TPersistent; PropNode: IXMLNode; PProp:
      PPropInfo);
  protected
    procedure ReadObject(Obj: TObject; const PropNode: IXMLNode); virtual;
    procedure ReadPersistent(Obj: TPersistent; Node: IXMLNode); virtual;
    function ReadValueFromXML(NOde: IXMLNode): variant;
  public
    procedure LoadXMLFormFile(const FileName: string);
    procedure LoadXMLString(const XML: wideString);
    function ReadXmlToObject(const ObjectName: string; Obj: TPersistent): Boolean;
    function ReadXMLToValue(Name: string): variant;
  end;

  // Read a xml to custom object (TPictrue, TGraphic, TStream...)
  TMyCustomReader = class(TCnXMLObjectReader)
  private
    procedure ReadPicture(const Pic: TPicture; const Node: IXMLNode);
    procedure ReadStream(Stream: TStream; Node: IXMLNode);
    procedure ReadStrings(Obj: TStrings; Node: IXMLNode);
  protected
    procedure ReadObject(Obj: TObject; const PropNode: IXMLNode); override;
    procedure ReadPersistent(Obj: TPersistent; Node: IXMLNode); override;
  end;

  // Custom writer write custom object(TPictrue, TGraphic, TStream...) to a xml
  TMyCustomWriter = class(TCnXMLObjectWriter)
  private
    procedure SaveGraphic(Obj: TGraphic; Node: IXMLNode);
    procedure SaveStream(Stream: TStream; Node: IXMLNode);
    procedure SaveTStrins(const Obj: TStrings; const Node: IXMLNode);
  protected
    procedure WriteObject(Prop: PPropInfo; Node: IXMLNode; Obj: TObject); override;
    procedure WritePersistentObject(const Obj: TPersistent; Node: IXMLNode);
      override;
  public
    procedure SavePicture(Pic: TPicture; Node: IXMLNode);
  end;

procedure ErrorCheck(const ErrorMessage: string);

implementation

uses
  Windows, SysUtils, Dialogs, CnDynObjBuilder, EncdDecd;

procedure ErrorCheck(const ErrorMessage: string);
begin
  raise Exception.Create(ErrorMessage);
end;

{ TCnXMLObjectWriter }

constructor TCnXMLObjectWriter.Create(AOwner: TComponent);
begin
  inherited;
  (*TODO: extracted code
  Self.InitXMLHead;
  FXMLDoc.Encoding :='UTF-8';
  *)
end;

procedure TCnXMLObjectWriter.LoadFromXMLString(XML: wideString);
begin
  FXMLDoc.XML.Clear;
  FXMLDoc.XML.Text := XML;
  FXMLDoc.Active := true;
  FRootNode := FXMLDoc.ChildNodes.FindNode(ROOT_OBJECT);
  if FRootNode = nil then
  begin
    InitXMLHead;
  end;
end;

procedure TCnXMLObjectWriter.WritePersistentObject(const Obj: TPersistent; Node:
  IXMLNode);
begin
  if Obj is TCollection then
  begin
    SavelCollectionItem(Obj, Node);
    Exit;
  end;
end;

procedure TCnXMLObjectWriter.WriteProperties(const Obj: TPersistent; Prop:
  PPropInfo; Node: IXMLNode);
var
  PropObj: TObject;
  IPropObj: Iinterface;
begin
  case Prop.PropType^.Kind of
    tkClass:
      begin
        PropObj := GetObjectProp(Obj, Prop.Name);
        if PropObj is TPersistent then
        begin
          WriteXMLData(TPersistent(PropObj), Node.ChildNodes.Nodes[Prop.Name]);
          Exit;
        end
        else
        begin
          WriteObject(Prop, Node, PropObj);
        end;
      end;
    tkInterface:
      begin
        IPropObj := GetInterfaceProp(Obj, Prop.Name);
        //
      end;
    tkMethod:
      begin
        Node.Attributes[Prop.Name] := GetPropValue(Obj, Prop.Name);
      end;
    tkEnumeration:
      begin
        Node.Attributes[Prop.Name] := GetEnumProp(Obj, Prop.Name);
      end;
    tkSet:
      begin
        Node.Attributes[Prop.Name] := GetSetProp(Obj, Prop.Name);
      end;
    tkUnknown, tkInteger, tkChar, tkFloat, tkWChar, tkVariant, tkInt64:
      begin
        Node.Attributes[Prop.Name] := GetPropValue(Obj, Prop.Name);
      end;
    tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
      begin
        Node.Attributes[Prop.Name] := GetPropValue(Obj, Prop.Name);
      end;
    tkArray, tkDynArray:
      begin
        Node.Attributes[Prop.Name] := GetPropValue(Obj, Prop.Name);
      end;
    tkRecord:
      begin
        Node.Attributes[Prop.Name] := GetPropValue(Obj, Prop.Name);
      end;
  end;
end;

procedure TCnXMLObjectWriter.SavelCollectionItem(const Obj: TPersistent; const
  Node: IXMLNode);
var
  I: Integer;
  PropObj: TObject;
  Child: IXMLNode;
begin
  if Obj is TCollection then
  begin
    for I := 0 to (Obj as TCollection).Count - 1 do
    begin
      PropObj := (Obj as TCollection).Items[I];
      Child := node.AddChild('Item');
      WriteXMLData((PropObj as TPersistent), Child);
    end;
  end;
end;

procedure TCnXMLObjectWriter.SaveXMLFile(XMLFileName: string);
begin
  FXMLDoc.SaveToFile(XMLFileName);
end;

procedure TCnXMLObjectWriter.SetClassType(const Obj: TPersistent; Nde: IXMLNode);

begin
  Nde.Attributes['ClassType'] := Obj.ClassName;
end;

procedure TCnXMLObjectWriter.SetPersistentType(const Obj: TPersistent; const
  Node: IXMLNode);
begin
  if Obj is TCollection then
  begin
    Node.Attributes['PersistentType'] := 'TCollection';
  end
  else
  begin
    if Obj is TCollectionItem then
    begin
      Node.Attributes['PersistentType'] := 'TCollectionItem';
    end
    else
    begin
      if Obj is TComponent then
      begin
        Node.Attributes['PersistentType'] := 'TComponent';
      end
      else
      begin
        if Obj is TGraphic then
        begin
          Node.Attributes['PersistentType'] := 'TGraphic';
        end
        else
        begin
          if Obj is TPicture then
          begin
            Node.Attributes['PersistentType'] := 'TPicture';
          end
          else
          begin
            Node.Attributes['PersistentType'] := 'TPersistent';
          end;
        end;
      end;
    end;
  end;
end;

procedure TCnXMLObjectWriter.WriteObject(Prop: PPropInfo; Node: IXMLNode; Obj:
  TObject);
begin

end;

procedure TCnXMLObjectWriter.WriteObjectToXML(ObjectName: string; const Obj:
  TPersistent);
var
  Node: IXMLNode;
begin
  //InitXMLHead;
  FXMLDoc.Encoding := Encoding;
  if not NodeExists(ObjectName) then
  begin
    Node := FRootNode.AddChild(ObjectName);
    Node.Attributes['NODE_TYPE'] := 'OBJECT';
  end
  else
  begin
    Node := FindNode(ObjectName);
    Node.AttributeNodes.Clear;
    Node.ChildNodes.Clear;
    Node.Attributes['NodeTYPE'] := 'OBJECT';
  end;
  WriteXMLData(Obj, Node);
end;

procedure TCnXMLObjectWriter.WriteValue(ParentNode: IXMLNode; const Name: string;
  const Value: Variant);
begin
  ParentNode.NodeValue := Value;
  ParentNode.Attributes['VarType'] := VarTypeAsText(VarType(Value));
end;

procedure TCnXMLObjectWriter.WriteValueToXML(Name: string; const Value: Variant);
var
  Node: IXMLNode;
begin
  //InitXMLHead;
  if not NodeExists(Name) then
  begin
    Node := FRootNode.AddChild(Name);
    Node.Attributes['NODE_TYPE'] := 'VALUE';
  end
  else
  begin
    Node := FindNode(Name);
    Node.AttributeNodes.Clear;
    Node.ChildNodes.Clear;
    Node.Attributes['NODE_TYPE'] := 'VALUE';
  end;
  WriteValue(Node, Name, Value);
end;

procedure TCnXMLObjectWriter.WriteXMLData(const Obj: TPersistent; const Nde:
  IXMLNode);
var
  CurNde, Node: IXMLNode;
  I: Integer;
  PList: PPropList;
  intPropCount: Integer;
  PPInfo: PPropInfo;
begin
  if Nde = nil then
  begin
    curNde := Self.FRootNode;
  end
  else
  begin
    curNde := Nde;
  end;
  Node := CurNde;

  Self.SetPersistentType(obj, Node);
  SetClassType(Obj, Node);

  //Save Collection
  Self.WritePersistentObject(Obj, Node);
  intPropCount := GetTypeData(Obj.ClassInfo)^.PropCount;
  GetMem(PList, intPropCount * SizeOf(Pointer));
  try
    intPropCount := GetPropList(Obj.ClassInfo, tkAny, PList);
    for I := 0 to intPropCount - 1 do
    begin
      PPInfo := PList^[I];

      WriteProperties(Obj, PPInfo, Node);
    end;
  finally
    FreeMem(PList, intPropCount * SizeOf(Pointer));
  end;
end;

procedure TCnXMLObjectReader.FindRootNode;
var
  nde: IXMLNode;
begin
  nde := FXMLDoc.ChildNodes.FindNode(ROOT_OBJECT);
  if nde = nil then raise Exception.Create('Object XML stream load error!');

  FRootNode := nde;
end;

function TCnXMLObjectReader.PropIsReadOnly(Pinfo: PPropInfo): Boolean;
begin
  Result := Pinfo.SetProc = nil;
end;

function TCnXMLObjectReader.PropIsValue(Pinfo: PPropInfo): Boolean;
begin
  Result := PInfo.PropType^.Kind in tkValue
end;

procedure TCnXMLObjectReader.ReadCollection(Collection: TCollection; Node:
  IXMLNode);
var
  I: integer;
  Child: IXMLNode;
  FItem: TCollectionItem;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName = 'Item' then
    begin
      Child := Node.ChildNodes[I];
      FItem := TCnDynamicBuilder.BuildCollectionItem(Child.Attributes['ClassType'], Collection);
      ReadPersistentFromXML(Child, FItem);
    end;
  end;
end;

procedure TCnXMLObjectReader.ReadPersistentFromXML(Node: IXMLNode; Instance:
  TPersistent);
var
  I: Integer;
  intPropCount: integer;
  Pprop: PPropInfo;
  PropNode: IXMLNode;
  PList: PPropList;
begin
  // process Collection
 (* if instance is TCollection then
  begin
    ReadCollection((Instance as TCollection),Node);
  end; *)
  ReadPersistent(Instance, Node);
  intPropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  GetMem(PList, intPropCount * SizeOf(Pointer));
  try

    for I := 0 to GetPropList(Instance.ClassInfo, tkValue, PList, False) - 1 do
    begin
      Pprop := PList^[I];
      if not PropIsReadOnly(PProp) then
      begin
        PropNode := Node.AttributeNodes.FindNode(PProp.Name);
        if PropNode <> nil then
        begin
          SetXMLPropValue(Instance, PropNode, PProp);
        end;
      end;
    end;

    for I := 0 to GetPropList(Instance.ClassInfo, tkObj, PList, False) - 1 do
    begin
      Pprop := Plist^[I];
      if Pprop.PropType^.Kind = tkClass then
      begin
        PropNode := Node.ChildNodes.FindNode(PProp.Name);
        if PropNode <> nil then
        begin
          ReadXmlObject(Instance, PropNode, Pprop);
        end;
      end
      else
      begin
        // todo: Interface Support
      end;
    end;
  finally
    FreeMem(PList, intPropCount * Sizeof(Pointer));
  end;
end;

procedure TCnXMLObjectReader.BuildInterfaceFromXML(Node: IXMLNode; Instance:
  IInterface);
begin
 { // Process StreamPersist
  if Supports(Instance,IID_IStreamPersist) then
  begin
    if not VarIsNull(Node.NodeValue) then
      Self.ReadIStreamPersist((Instance as IStreamPersist),Node);
  end;

  intPropCount :=GetPropList(Instance.classInfo,tkValue,@PropList,False);

  for I := 0 to intPropCount-1 do
  begin
    if  not PropIsReadOnly(PropList[I])) then
    begin
      PropNode :=Node.AttributeNodes.FindNode(PropList[I].Name);
      if PropNode <>nil then
      begin
        SetXMLPropValue(Instance,PropNode);
      end;
    end;
  end;
  // Interface Class
  intPropCount :=GetPropList(Instance.ClassInfo,[tkInterface],@PropList,False);
  for I:=0 to intPropCount-1  do
  begin
      PropNode := Node.ChildNodes.FindNode(PropList[I].Name);
      if PropNode<>nil then
      begin
        PropObj :=(GetObjectProp(Instance,PropNode.NodeName) as IIterface);
        BuildInterfaceFromXML(PropNode,PropObj);
      end;
      end;
      end;
  end;   }
end;

procedure TCnXMLObjectReader.LoadXMLFormFile(const FileName: string);
begin
  Self.FXMLDoc.LoadFromFile(FileName);
  Self.Init;
end;

procedure TCnXMLObjectReader.LoadXMLString(const XML: wideString);

begin
  Self.FXMLDoc.XML.Clear;
  Self.FXMLDoc.XML.Text := XML;
  Self.FXMLDoc.Active := true;
  Init;
end;

procedure TCnXMLObjectReader.ReadObject(Obj: TObject; const PropNode: IXMLNode);
begin
 //
end;

procedure TCnXMLObjectReader.ReadPersistent(Obj: TPersistent; Node: IXMLNode);
begin
  if Obj is TCollection then
  begin
    ReadCollection((Obj as TCollection), Node);
    Exit;
  end;
end;

function TCnXMLObjectReader.ReadValueFromXML(NOde: IXMLNode): variant;
begin
  Result := Node.NodeValue;
end;

function TCnXMLObjectReader.ReadXMLToValue(Name: string): variant;
var
  Node: IXMLNode;
begin
  if NodeExists(Name) then
  begin
    Node := FindNode(Name);
    if not (Node.Attributes['NODE_TYPE'] = 'VALUE') then
      raise Exception.Create('NODE_TYPE is Error ' + Node.Attributes['NODE_TYPE']);
    Result := ReadValueFromXML(Node);
  end
  else
  begin
    Result := Unassigned;
  end;
end;

procedure TCnXMLObjectReader.SetObjPropValue(PropName: string; Instance,
  PropObj: TPersistent);
begin
  SetObjectProp(Instance, PropName, PropObj);
end;

procedure TCnXMLObjectReader.ReadXMLObject(Instance: TPersistent; PropNode:
  IXMLNode; PProp: PPropInfo);
var
  PropObj: TObject;
begin
  PropObj := GetObjectProp(Instance, PProp.Name);
  if PropObj is TPersistent then
  begin
    ReadPersistentFromXML(PropNode, TPersistent(PropObj));
  end
  else
  begin
    ReadObject(PropObj, PropNode);
  end;
end;

function TCnXMLObjectReader.ReadXmlToObject(const ObjectName: string; Obj:
  TPersistent): Boolean;
var
  Node: IXMLNode;
begin
  if NodeExists(ObjectName) then
  begin
    Node := FindNode(ObjectName);
    if CompareText(Obj.ClassName, node.Attributes['ClassType']) = 0 then
    begin
      ReadPersistentFromXML(Node, Obj);
      Result := true;
    end
    else
    begin
      Result := false;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TCnXMLObjectReader.SetXMLPropValue(Obj: TPersistent; PropNode:
  IXMLNode; PProp: PPropInfo);
begin
  case PProp.PropType^.Kind of
    tkEnumeration:
      begin
        SetEnumProp(Obj, PProp.Name, PropNode.NodeValue);
      end;
    tkMethod:
      begin
        // SetMethodProp();
        // SetMethodProp(Obj,PProp.Name);
      end;
  else
    begin
      SetPropValue(Obj, PProp.Name, PropNode.NodeValue);
    end;
  end;
end;

{ TCnXMLObjectFiler }

constructor TCnXMLObjectFiler.Create(AOwner: TComponent);
begin
  inherited;
  FXMLDoc := TXMLDocument.Create(Self);
end;

destructor TCnXMLObjectFiler.Destroy;
begin
  FreeAndNil(FXmLDoc);
  inherited;
end;

function TCnXMLObjectFiler.FindNode(const NodeName: string): IXMLNode;
begin
  Result := FRootNode.ChildNodes.FindNode(NodeName);
end;

function TCnXMLObjectFiler.GetXMLString: string;
begin
  Result := Self.FXMLDoc.XML.Text;
end;

procedure TCnXMLObjectFiler.Init;
begin
  if not FXMLDoc.Active then FXMLDoc.Active := true;
  if FXMLDoc.ChildNodes.FindNode(ROOT_OBJECT) = nil then
    Self.InitXMLHead
  else
  begin
    FRootNode := FXMLDoc.ChildNodes.FindNode(ROOT_OBJECT);
  end;

  FXMLDoc.Encoding := 'UTF-8';
end;

procedure TCnXMLObjectFiler.InitXMLHead;
begin
  FXMLDoc.XML.Clear;
  FXMLDoc.XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
  FXMLDoc.XML.Add('<' + ROOT_OBJECT + '></' + ROOT_OBJECT + '>');
  FXMLDoc.Active := true;
  FRootNode := FXMLDoc.ChildNodes.FindNode(ROOT_OBJECT);
  if FRootNode = nil then raise Exception.Create('Not found ' + Root_OBJECT);
end;

function TCnXMLObjectFiler.NodeExists(const NodeName: string): Boolean;
begin
  Result := FRootNode.ChildNodes.FindNode(NodeName) <> nil;
end;

procedure TCnXMLObjectFiler.SetXMLString(const Value: string);
begin
  FXMLDoc.XML.Text := Value;
end;

procedure TMyCustomReader.ReadObject(Obj: TObject; const PropNode: IXMLNode);
begin
  inherited;
  if Obj is TStream then
  begin
    ReadStream(TStream(Obj), PropNode);
  end;
end;

procedure TMyCustomReader.ReadPersistent(Obj: TPersistent; Node: IXMLNode);
begin
  inherited;
  if Obj is TStrings then
  begin
    Self.ReadStrings(TStrings(Obj), Node);
  end;
  if Obj is TPicture then
  begin
    Self.ReadPicture(TPicture(Obj), Node);
    Exit;
  end;
end;

procedure TMyCustomReader.ReadPicture(const Pic: TPicture; const Node:
  IXMLNode);
var
  str: TStringStream;
  MemStream: TMemoryStream;
begin
  str := TStringStream.Create(Node.NodeValue);
  MemStream := TMemoryStream.Create;
  try
    str.Position := 0;
    DecodeStream(str, MemStream);
    MemStream.Position := 0;
    Pic.Bitmap.LoadFromStream(MemStream);
  finally
    FreeAndNil(str);
    FreeAndNil(MemStream);
  end;
end;

procedure TMyCustomReader.ReadStream(Stream: TStream; Node: IXMLNode);
var
  str: TStringStream;
begin
  str := TStringStream.Create(Node.NodeValue);
  try
    str.Position := 0;
    Stream.Position := 0;
    DecodeStream(str, Stream);
  finally
    FreeAndNil(str);
  end;
end;

procedure TMyCustomReader.ReadStrings(Obj: TStrings; Node: IXMLNode);
var
  I: integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    Obj.Add(Node.ChildNodes[I].NodeValue);
  end;
end;

procedure TMyCustomWriter.SaveGraphic(Obj: TGraphic; Node: IXMLNode);
var
  Stream: TMemoryStream;
  str: TStringStream;
begin
  if Obj.Empty then Exit;
  Stream := TMemoryStream.Create;
  str := TStringStream.Create('');
  try
    str.Position := 0;
    Obj.SaveToStream(Stream);
    Stream.Position := 0;
    EncodeStream(Stream, str);
    Node.NodeValue := str.DataString;
  finally
    FreeAndNil(str);
    FreeAndNil(Stream);
  end;
end;

procedure TMyCustomWriter.SavePicture(Pic: TPicture; Node: IXMLNode);
var
  Stream: TMemorystream;
  str: TStringStream;
begin
  Stream := TMemoryStream.Create();
  str := TStringStream.Create('');
  try
    str.Position := 0;
    if Assigned(Pic) then
    begin
      Pic.Bitmap.SaveToStream(stream);
      Stream.Position := 0;
      EncodeStream(Stream, str);
      Node.NodeValue := str.DataString;
    end;
  finally
    FreeAndNil(str);
    FreeAndNil(Stream);
  end;
end;

procedure TMyCustomWriter.SaveStream(Stream: TStream; Node: IXMLNode);
var
  str: TStringStream;
begin
  str := TStringStream.Create('');
  try
    str.Position := 0;
    Stream.Position := 0;
    EncodeStream(Stream, str);
    Node.NodeValue := str.DataString;
  finally
    FreeAndNil(str);
  end;
end;

procedure TMyCustomWriter.SaveTStrins(const Obj: TStrings; const Node:
  IXMLNode);
var
  I: integer;
  Child: IXMLNode;
begin
  for I := 0 to Obj.Count - 1 do
  begin
    Child := Node.AddChild('StringItem');
    Child.NodeValue := Obj.Strings[I];
  end;
end;

procedure TMyCustomWriter.WriteObject(Prop: PPropInfo; Node: IXMLNode; Obj:
  TObject);
begin
  inherited;

  if Obj is TStream then
  begin
    SaveStream(TStream(Obj), Node.ChildNodes.Nodes[Prop.Name]);
    Exit;
  end;
end;

procedure TMyCustomWriter.WritePersistentObject(const Obj: TPersistent; Node:
  IXMLNode);
begin
  inherited;
  if Obj is TStrings then
  begin
    SaveTStrins(TStrings(Obj), Node);
    Exit;
  end;
  if obj is TGraphic then
  begin
    SaveGraphic(TIcon(obj), Node);
    Exit;
  end;

  if Obj is TPicture then
  begin
    SavePicture(TPicture(Obj), Node);
    Exit;
  end;
end;

end.
