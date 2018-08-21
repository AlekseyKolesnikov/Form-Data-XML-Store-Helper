unit FormDataXmlStore;

{ A simple form data store helper to xml for Delphi.
  Copyright (C) 2018 by Alexey Kolesnikov.
  Email: ak@blu-disc.net
  Website: https://blu-disc.net

You can use this software for whatever you want.

Usage:

  Create, add property names for storing, call Save/Load.

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
}

interface

uses
  SysUtils, Classes, Controls, Forms, xmldom, msxmldom, XMLIntf, XMLDoc, TypInfo;

type
  TFormDataXmlStoreCommand = (apcSave, apcLoad);

  TFormDataXmlStorePropProc = procedure(Control: TControl; Node: IXMLNode; XmlStoreCommand: TFormDataXmlStoreCommand);

  TFormDataXmlStore = class
  private
    Form: TForm;
    RequiredProperties, OptionalProperties, CustomProperties: TStringList;
    xml: TXMLDocument;
    FSaveVisible: Boolean;

    function AddNode(Control: TControl; ParentNode: IXMLNode): IXMLNode;
    procedure ClearXml;
    function GetPropertyCustomIndex(Control: TControl; aProperty: String): Integer;
    function GetPropertyIndex(Control: TControl; aProperty: String; List: TStringList): Integer;
    function GetPropertyName(Control: TControl; aProperty: String): String;
    function HasAnyProperty(Control: TControl): Boolean;
    function IsPropertyAllowed(Control: TControl; aProperty: String): Boolean;
    procedure LoadControl(Control: TControl; Node: IXMLNode);
    procedure SaveControl(Control: TControl; ParentNode: IXMLNode);
    function SaveProperty(Control: TControl; aProperty: String; node: IXMLNode): Boolean;
    function SavePropertyCustom(Control: TControl; iProperty: Integer; node: IXMLNode): Boolean;
  public
    property SaveVisible: Boolean read FSaveVisible write FSaveVisible;

    constructor Create(aForm: TForm);
    destructor Destroy; override;

    procedure AddPropertyCustom(aName: String; StoreProc: TFormDataXmlStorePropProc);
    procedure AddPropertyOptional(aName: String);
    procedure AddPropertyRequired(aName: String);
    procedure Load(FileName: String);
    procedure Save(FileName: String);
  end;

implementation

uses
  Variants;


{ TXmlStore }

function TFormDataXmlStore.AddNode(Control: TControl; ParentNode: IXMLNode): IXMLNode;
begin
  Result := ParentNode.AddChild('node');
  Result.Attributes['Name'] := Control.Name;
  if FSaveVisible then
    Result.Attributes['Visible'] := Control.Visible;
end;

procedure TFormDataXmlStore.AddPropertyCustom(aName: String; StoreProc: TFormDataXmlStorePropProc);
begin
  CustomProperties.AddObject(aName, @StoreProc);
end;

procedure TFormDataXmlStore.AddPropertyOptional(aName: String);
begin
  OptionalProperties.Add(aName);
end;

procedure TFormDataXmlStore.AddPropertyRequired(aName: String);
begin
  RequiredProperties.Add(aName);
end;

procedure TFormDataXmlStore.ClearXml;
begin
  xml.LoadFromXML('<?xml version="1.0" encoding="utf-8"?>'#13#10 +
    '<root xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'#13#10 +
    '</root>'#13#10);
  xml.Encoding := 'utf-8';
end;

constructor TFormDataXmlStore.Create(aForm: TForm);
begin
  inherited Create;

  FSaveVisible := False;
  Form := aForm;

  xml := TXMLDocument.Create(aForm);
  xml.DOMVendor := GetDOMVendor(SMSXML);
  xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl];

  ClearXml;

  RequiredProperties := TStringList.Create;
  OptionalProperties := TStringList.Create;
  CustomProperties := TStringList.Create;
end;

destructor TFormDataXmlStore.Destroy;
begin
  xml.Free;
  RequiredProperties.Free;
  OptionalProperties.Free;
  CustomProperties.Free;

  inherited;
end;

function TFormDataXmlStore.GetPropertyCustomIndex(Control: TControl; aProperty: String): Integer;
begin
  Result := GetPropertyIndex(Control, aProperty, CustomProperties);
end;

function TFormDataXmlStore.GetPropertyIndex(Control: TControl; aProperty: String; List: TStringList): Integer;
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

function TFormDataXmlStore.GetPropertyName(Control: TControl; aProperty: String): String;
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

function TFormDataXmlStore.IsPropertyAllowed(Control: TControl; aProperty: String): Boolean;
begin
  Result := GetPropertyIndex(Control, aProperty, RequiredProperties) > -1;
  if not Result then
    Result := GetPropertyIndex(Control, aProperty, OptionalProperties) > -1;
end;

procedure TFormDataXmlStore.Load(FileName: String);
begin
  xml.LoadFromFile(FileName);
  LoadControl(Form, xml.ChildNodes['root'].ChildNodes['node']);
  ClearXml;
end;

procedure TFormDataXmlStore.LoadControl(Control: TControl; Node: IXMLNode);
var
  iAttribute, iNode, iProperty: Integer;
  childNode: IXMLNode;
  childControl: TControl;
  Attribute: string;
begin
  for iAttribute := 0 to Node.AttributeNodes.Count - 1 do
  begin
    Attribute := Node.AttributeNodes[iAttribute].NodeName;

    if (Attribute <> 'Name') and IsPublishedProp(Control, Attribute) then
    begin
      if IsPropertyAllowed(Control, Attribute) then
        SetPropValue(Control, Attribute, VarToStr(Node.AttributeNodes[iAttribute].NodeValue))
      else
      begin
        iProperty := GetPropertyCustomIndex(Control, Attribute);
        if iProperty > -1 then
          TFormDataXmlStorePropProc(CustomProperties.Objects[iProperty])(Control, Node, apcLoad);
      end;
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
        childControl := TWinControl(Control).FindChildControl(childNode.Attributes['Name']);
        if Assigned(childControl) then
          LoadControl(childControl, childNode);
      end;
    end;
end;

procedure TFormDataXmlStore.Save(FileName: String);
begin
  SaveControl(Form, xml.ChildNodes['root']);
  xml.SaveToFile(FileName);
  ClearXml;
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
    node.Attributes[aProperty] := GetPropValue(Control, aProperty);
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
    node.Attributes[aProperty] := 'custom';
    TFormDataXmlStorePropProc(CustomProperties.Objects[iProperty])(Control, node, apcSave);
    Result := True;
  end;
end;

end.

