# from issue https://github.com/geany/geany/issues/612

import colander

class DeviceSchema(colander.MappingSchema):
    resourceType = colander.SchemaNode(
        colander.String(),
        description='ResourceType informs the parser which resource type this is',
        missing='Device',
        default='Device',
        validator=colander.Function(lambda x: x == 'Device', msg="resourceType can't be changed")
    )
    identifier = colander.SchemaNode(
        colander.String(),
        description='Instance id from manufacturer, owner and others',
        missing=colander.drop,
        default=colander.drop,
    )
    type = colander.SchemaNode(
        colander.String(),
        description="What kind of device this is",
        missing=colander.required,
    )
    manufacturer = colander.SchemaNode(
        colander.String(),
        description='Name of device manufacturer',
        missing=colander.drop,
    )
