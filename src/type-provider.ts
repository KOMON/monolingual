import type { Document } from 'mongodb';

export interface SchemaTypeProvider {
  readonly baseSchemaType: unknown;
  readonly baseCompiledSchemaType: unknown;

  readonly schemaType: unknown;
  readonly compiledSchemaType: unknown;
  
  readonly resolveSchemaType: Document;
  readonly resolveCompiledSchemaType: Document;
}

export type DefaultSchemaTypeProvider = SchemaTypeProvider;

export type BaseSchemaType<TypeProvider extends SchemaTypeProvider> =
  TypeProvider['baseSchemaType'];

export type BaseCompiledSchemaType<TypeProvider extends SchemaTypeProvider> =
  TypeProvider['baseCompiledSchemaType'];

export type ResolveSchemaType<
  TypeProvider extends SchemaTypeProvider, 
  S extends TypeProvider['baseSchemaType']
> = (TypeProvider & { schemaType: S})['resolveSchemaType'];

export type ResolveCompiledType<
  TypeProvider extends SchemaTypeProvider, 
  C extends TypeProvider['baseCompiledSchemaType']
> = (TypeProvider & { compiledSchemaType: C})['resolveCompiledSchemaType'];

export type ResolveCompiledSchema<
  TypeProvider extends SchemaTypeProvider, 
  S extends TypeProvider['baseSchemaType']
> = (TypeProvider & { schemaType: S})['compiledSchemaType'];

export interface SchemaProvider<TypeProvider extends SchemaTypeProvider> {
  compile<S extends TypeProvider['baseSchemaType']>(
    schema: S
  ): ResolveCompiledSchema<TypeProvider, S>

  validate<C extends TypeProvider['baseCompiledSchemaType']>(
    compiledSchema: C, 
    u: unknown
  ): u is ResolveCompiledType<TypeProvider, C>;
}
