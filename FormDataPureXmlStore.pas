unit FormDataPureXmlStore;

interface

{$region 'Description'} {

A simple form data to xml store helper for Delphi.
Copyright (C) 2018 by Alexey Kolesnikov.
Email: ak@blu-disc.net
Website: https://blu-disc.net

You can use this software for whatever you want.


Usage:

  Create, add property names for storing, call Save/Load.
  Optionally you can make some actions before restoring (OnLoad event called after xml loaded, before restoring)
  and before saving (OnSave event called after xml created, before saving). Both methods give you the form root node.

  Property name can be:
    - Simple property name, like ItemIndex, Checked, Value, Text, etc.
    - ControlName|Property, like ComboBox1|Text
    - ClassName.Property, like TMemo.Lines

  You can store properties using three groups:
    - Required group: stores property always if found.
    - Custom group: stores the property by calling the specified procedure (TFormDataXmlStorePropProc).
    - Optional group: stores this property only if nothing has been found in the Required and Custom lists for this control.


Example of usage:

  XmlStore := TFormDataXmlStore.Create(aForm);
  XmlStore.OnLoad := XmlStore_OnLoad; // optional
  XmlStore.OnSave := XmlStore_OnSave; // optional
  XmlStore.AddPropertyRequired('Checked');   // will store Checked for all controls that have it
  XmlStore.AddPropertyRequired('ItemIndex'); // will store ItemIndex for all controls that have it
  XmlStore.AddPropertyRequired('cmbProjectLangCode|Text'); // will store Text for cmbProjectLangCode
  XmlStore.AddPropertyCustom('TMemo.Lines', XmlStoreMemoLines); // will store Lines for all TMemo using XmlStoreMemoLines procedure
  XmlStore.AddPropertyOptional('Text'); // will store Text for all controls that have it, and does not have anything from other lists

  ...

  XmlStore.Save(FileName);

  ...

  XmlStore.Load(FileName);

  ...

  procedure XmlStoreMemoLines(Control: TControl; Node: IXMLNode; XmlStoreCommand: TFormDataXmlStoreCommand);
  begin
    if XmlStoreCommand = apcSave then
      Node.AddChild('Lines').NodeValue := TMemo(Control).Lines.Text
    else
      TMemo(Control).Lines.Text := VarToStr(Node.ChildNodes['Lines'].NodeValue);
  end;

  ...

  procedure TfrmWizard.XmlStore_OnLoad(RootNode: IXMLNode);
  begin
    if RootNode.HasAttribute('ProjectFolder') then
      ProjectFolder := RootNode.Attributes['ProjectFolder'];
  end;

  procedure TfrmWizard.XmlStore_OnSave(RootNode: IXMLNode);
  begin
    RootNode.Attributes['ProjectFolder'] := ProjectFolder;
  end;


Revision history:

  21-Aug-2018 initial release
  23-Aug-2018 changed load order to Custom, Required, Optional
  29-Aug-2018 OnLoad/OnSave added, refactoring
  30-Sep-2018 GetNode added - returns node using array Path (useful to patch old xmls in onLoad)

} {$endregion}

uses
  SysUtils, Classes, Controls, Forms, PureXML, TypInfo;

type
  TFormDataXmlStoreCommand = (apcSave, apcLoad);

  TFormDataXmlStorePropProc = procedure(Control: TControl; Node: IXMLNode; XmlStoreCommand: TFormDataXmlStoreCommand) of object;

  TFormDataXmlStoreNotify = procedure(RootNode: IXMLNode) of object;

  TFormDataXmlStoreCustomData = class
    StoreProc: TFormDataXmlStorePropProc;
    constructor Create(aStoreProc: TFormDataXmlStorePropProc);
  end;

  TFormDataXmlStore = class
  private
    Form: TForm;
    RequiredProperties, OptionalProperties, CustomProperties: TStringList;
    xml: TXMLDocument;
    FSaveVisible: Boolean;
    FOnLoad: TFormDataXmlStoreNotify;
    FOnSave: TFormDataXmlStoreNotify;

    function AddNode(Control: TControl; ParentNode: IXMLNode): IXMLNode;
    function FindNode(root: IXMLNode; NodeName: String): IXMLNode;
    function GetPropertyIndex(Control: TControl; const aProperty: String; List: TStringList): Integer;
    function GetPropertyName(Control: TControl; const aProperty: String): String;
    function HasAnyProperty(Control: TControl): Boolean;
    procedure LoadControl(Control: TControl; Node: IXMLNode);
    procedure SaveControl(Control: TControl; ParentNode: IXMLNode);
    function SaveProperty(Control: TControl; aProperty: String; node: IXMLNode): Boolean;
    function SavePropertyCustom(Control: TControl; iProperty: Integer; node: IXMLNode): Boolean;
  public
    property SaveVisible: Boolean read FSaveVisible write FSaveVisible;
    property OnLoad: TFormDataXmlStoreNotify read FOnLoad write FOnLoad;
    property OnSave: TFormDataXmlStoreNotify read FOnSave write FOnSave;

    constructor Create(aForm: TForm);
    destructor Destroy; override;

    procedure AddPropertyCustom(const aName: String; StoreProc: TFormDataXmlStorePropProc);
    procedure AddPropertyOptional(const aName: String);
    procedure AddPropertyRequired(const aName: String);
    function GetNode(RootNode: IXMLNode; Path: array of String; Index: Integer = 0): IXMLNode;
    procedure Load(const FileName: String);
    procedure LoadArray(const FileName: String; Controls: array of TWinControl);
    procedure Save(const FileName: String);
    procedure SaveArray(const FileName: String; Controls: array of TWinControl);
  end;


function FindNodeByName(Node: IXMLNode; Name: String): IXMLNode;
function FindNodeByNameChain(Node: IXMLNode; Names: array of String): IXMLNode;


implementation

uses
  Variants;

function FindNodeByName(Node: IXMLNode; Name: String): IXMLNode;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Node.ChildNodes.Count - 1 do
    if (Node.ChildNodes[i].NodeType = ntElement) and (Node.ChildNodes[i].Attributes('Name') = Name) then
    begin
      Result := Node.ChildNodes[i];
      Break;
    end;
end;

function FindNodeByNameChain(Node: IXMLNode; Names: array of String): IXMLNode;
var
  i: Integer;
begin
  Result := Node;

  for i := 0 to Length(Names) - 1 do
  begin
    Result := FindNodeByName(Result, Names[i]);
    if not Assigned(Result) then
      Break;
  end;
end;


{ TXmlStore }

function TFormDataXmlStore.AddNode(Control: TControl; ParentNode: IXMLNode): IXMLNode;
begin
  Result := ParentNode.AddChild('node');
  Result.Attributes('Name', Control.Name);
  if FSaveVisible then
    Result.Attributes('Visible', Control.Visible);
end;

procedure TFormDataXmlStore.AddPropertyCustom(const aName: String; StoreProc: TFormDataXmlStorePropProc);
begin
  CustomProperties.AddObject(aName, TFormDataXmlStoreCustomData.Create(StoreProc));
end;

procedure TFormDataXmlStore.AddPropertyOptional(const aName: String);
begin
  OptionalProperties.Add(aName);
end;

procedure TFormDataXmlStore.AddPropertyRequired(const aName: String);
begin
  RequiredProperties.Add(aName);
end;

constructor TFormDataXmlStore.Create(aForm: TForm);
begin
  inherited Create;

  FSaveVisible := False;
  Form := aForm;
  OnLoad := nil;
  OnSave := nil;

  xml := TXMLDocument.Create(aForm);

  RequiredProperties := TStringList.Create;
  OptionalProperties := TStringList.Create;
  CustomProperties := TStringList.Create(True);
end;

destructor TFormDataXmlStore.Destroy;
begin
  xml.Free;
  RequiredProperties.Free;
  OptionalProperties.Free;
  CustomProperties.Free;

  inherited;
end;

function TFormDataXmlStore.FindNode(root: IXMLNode; NodeName: String): IXMLNode;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to root.ChildNodes.Count - 1 do
    if (root.ChildNodes[i].NodeType = ntElement) and (root.ChildNodes[i].Attributes('Name') = NodeName) then
    begin
      Result := root.ChildNodes[i];
      Break;
    end;
end;

function TFormDataXmlStore.GetNode(RootNode: IXMLNode; Path: array of String; Index: Integer = 0): IXMLNode;
var
  i: Integer;
begin
  Result := nil;

  if Index >= Length(Path) then
    Exit;

  for i := 0 to RootNode.ChildNodes.Count - 1 do
    if RootNode.ChildNodes[i].Attributes('Name') = Path[Index] then
    begin
      if Index < Length(Path) - 1 then
        Result := GetNode(RootNode.ChildNodes[i], Path, Index + 1)
      else
        Result := RootNode.ChildNodes[i];
      Break;
    end;
end;

function TFormDataXmlStore.GetPropertyIndex(Control: TControl; const aProperty: String; List: TStringList): Integer;
var
  iProperty, i: Integer;
  ListProperty: string;
begin
  Result := -1;

  for iProperty := 0 to List.Count - 1 do
  begin
    ListProperty := GetPropertyName(Control, List[iProperty]);

    if ListProperty = aProperty then
    begin
      i := Pos('.', List[iProperty]);
      if i > 0 then
      begin
        if Copy(List[iProperty], 1, i - 1) = Control.ClassName then
          Result := iProperty;
      end
      else
      begin
        i := Pos('|', List[iProperty]);
        if i > 0 then
        begin
          if Copy(List[iProperty], 1, i - 1) = Control.Name then
            Result := iProperty;
        end
        else
          Result := iProperty;
      end;

      Exit;
    end;
  end;
end;

function TFormDataXmlStore.GetPropertyName(Control: TControl; const aProperty: String): String;
var
  i: Integer;
begin
  Result := aProperty;

  i := Pos('|', aProperty);
  if i > 0 then
  begin
    if Control.Name = Copy(aProperty, 1, i - 1) then
      Result := Copy(aProperty, i + 1, Length(aProperty) - i)
    else
      Result := '';

    Exit;
  end;

  i := Pos('.', aProperty);
  if i > 0 then
  begin
    if Control.ClassName = Copy(aProperty, 1, i - 1) then
      Result := Copy(aProperty, i + 1, Length(aProperty) - i)
    else
      Result := '';

    Exit;
  end;
end;

function TFormDataXmlStore.HasAnyProperty(Control: TControl): Boolean;
var
  iProperty: Integer;
  aProperty: string;
begin
  Result := False;

  for iProperty := 0 to RequiredProperties.Count - 1 do
  begin
    aProperty := GetPropertyName(Control, RequiredProperties[iProperty]);
    if (aProperty <> '') and IsPublishedProp(Control, aProperty) then
    begin
      Result := True;
      Exit;
    end;
  end;

  for iProperty := 0 to OptionalProperties.Count - 1 do
  begin
    aProperty := GetPropertyName(Control, OptionalProperties[iProperty]);
    if (aProperty <> '') and IsPublishedProp(Control, aProperty) then
    begin
      Result := True;
      Exit;
    end;
  end;

  for iProperty := 0 to CustomProperties.Count - 1 do
  begin
    aProperty := GetPropertyName(Control, CustomProperties[iProperty]);
    if (aProperty <> '') and IsPublishedProp(Control, aProperty) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TFormDataXmlStore.Load(const FileName: String);
begin
  xml.LoadFromFile(FileName);

  if Assigned(OnLoad) then
    OnLoad(xml.ChildNodes('root').ChildNodes('node'));

  LoadControl(Form, xml.ChildNodes('root').ChildNodes('node'));
  xml.Clear;
end;

procedure TFormDataXmlStore.LoadArray(const FileName: String; Controls: array of TWinControl);
var
  i: Integer;
  node: IXMLNode;
begin
  xml.LoadFromFile(FileName);

  if Assigned(OnLoad) then
    OnLoad(xml.ChildNodes('root'));

  for i := 0 to Length(Controls) - 1 do
  begin
    node := FindNode(xml.ChildNodes('root'), Controls[i].Name);
    if node <> nil then
      LoadControl(Controls[i], node);
  end;

  xml.Clear;
end;

procedure TFormDataXmlStore.LoadControl(Control: TControl; Node: IXMLNode);
var
  iAttribute, iNode, iProperty: Integer;
  childNode: IXMLNode;
  childControl: TControl;
  PropertyName: string;
begin
  for iAttribute := 0 to Node.AttributeNodes.Count - 1 do
  begin
    PropertyName := Node.AttributeNodes.Names[iAttribute];

    if (PropertyName <> 'Name') and IsPublishedProp(Control, PropertyName) then
    begin
      iProperty := GetPropertyIndex(Control, PropertyName, CustomProperties);
      if iProperty > -1 then
        TFormDataXmlStoreCustomData(CustomProperties.Objects[iProperty]).StoreProc(Control, Node, apcLoad)
      else
      if GetPropertyIndex(Control, PropertyName, RequiredProperties) > -1 then
        SetPropValue(Control, PropertyName, Node.AttributeNodes.ValueFromIndex[iAttribute])
      else
      if GetPropertyIndex(Control, PropertyName, OptionalProperties) > -1 then
        SetPropValue(Control, PropertyName, Node.AttributeNodes.ValueFromIndex[iAttribute]);
    end;
  end;

  if Control is TWinControl then
    for iNode := 0 to Node.ChildNodes.Count - 1 do
    begin
      childNode := Node.ChildNodes[iNode];
      if childNode.NodeName <> 'node' then
        Continue;

      if childNode.HasAttribute('Name') then
      begin
        childControl := TWinControl(Control).FindChildControl(childNode.Attributes('Name'));
        if Assigned(childControl) then
          LoadControl(childControl, childNode);
      end;
    end;
end;

procedure TFormDataXmlStore.Save(const FileName: String);
begin
  SaveControl(Form, xml.ChildNodes('root'));
  xml.SaveToFile(FileName);
  xml.Clear;
end;

procedure TFormDataXmlStore.SaveArray(const FileName: String; Controls: array of TWinControl);
var
  i: Integer;
begin
  for i := 0 to Length(Controls) - 1 do
    SaveControl(Controls[i], xml.ChildNodes('root'));

  xml.SaveToFile(FileName);
  xml.Clear;
end;

procedure TFormDataXmlStore.SaveControl(Control: TControl; ParentNode: IXMLNode);
var
  iControl, iProperty: Integer;
  node: IXMLNode;
  GotProp: Boolean;
begin
  if Control.Name = '' then
    Exit;

  if HasAnyProperty(Control) then
  begin
    node := AddNode(Control, ParentNode);
    GotProp := False;

    for iProperty := 0 to RequiredProperties.Count - 1 do
      if SaveProperty(Control, RequiredProperties[iProperty], node) then
        GotProp := True;

    for iProperty := 0 to CustomProperties.Count - 1 do
      if SavePropertyCustom(Control, iProperty, node) then
        GotProp := True;

    if not GotProp then
      for iProperty := 0 to OptionalProperties.Count - 1 do
        if SaveProperty(Control, OptionalProperties[iProperty], node) then
          Break;
  end
  else
    node := nil;

  if (Control = Form) and Assigned(OnSave) then
  begin
    if not Assigned(node) then
      node := AddNode(Control, ParentNode);
    OnSave(node);
  end;

  if (Control is TWinControl) and (TWinControl(Control).ControlCount > 0) then
  begin
    if not Assigned(node) then
      node := AddNode(Control, ParentNode);

    for iControl := 0 to TWinControl(Control).ControlCount - 1 do
      SaveControl(TWinControl(Control).Controls[iControl], node);
  end;
end;

function TFormDataXmlStore.SaveProperty(Control: TControl; aProperty: String; node: IXMLNode): Boolean;
begin
  Result := False;
  aProperty := GetPropertyName(Control, aProperty);
  if aProperty = '' then
    Exit;

  if IsPublishedProp(Control, aProperty) then
  begin
    node.Attributes(aProperty, GetPropValue(Control, aProperty));
    Result := True;
  end;
end;

function TFormDataXmlStore.SavePropertyCustom(Control: TControl; iProperty: Integer; node: IXMLNode): Boolean;
var
  aProperty: string;
begin
  Result := False;
  aProperty := GetPropertyName(Control, CustomProperties[iProperty]);
  if aProperty = '' then
    Exit;

  if IsPublishedProp(Control, aProperty) then
  begin
    node.Attributes(aProperty, 'custom');
    TFormDataXmlStoreCustomData(CustomProperties.Objects[iProperty]).StoreProc(Control, node, apcSave);
    Result := True;
  end;
end;


{ TFormDataXmlStoreCustomData }

constructor TFormDataXmlStoreCustomData.Create(aStoreProc: TFormDataXmlStorePropProc);
begin
  inherited Create;
  StoreProc := aStoreProc;
end;

end.

