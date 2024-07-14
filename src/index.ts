import { TSchema, Type, type Static} from '@sinclair/typebox';
import { TypeCompiler, type TypeCheck } from '@sinclair/typebox/compiler';
import { 
  Collection as MongoCollection, 
  type Document, 
  type AggregationCursor,
  type AggregateOptions,
  AnyBulkWriteOperation,
  BulkWriteOptions
} from 'mongodb';

import type { SchemaTypeProvider, SchemaProvider, ResolveCompiledSchema, ResolveSchemaType, BaseSchemaType} from './type-provider.js';

export type CollectionConfig<
  TypeProvider extends SchemaTypeProvider,
  ReadSchema extends BaseSchemaType<TypeProvider>,
  UpdateSchema extends BaseSchemaType<TypeProvider> = ReadSchema,
  CreateSchema extends BaseSchemaType<TypeProvider> = ReadSchema,
> = {
  readonly schemaProvider: SchemaProvider<TypeProvider>,
  readonly readSchema: ReadSchema,
  readonly updateSchema: UpdateSchema,
  readonly createSchema: CreateSchema,
}

export class Collection<
  TypeProvider extends SchemaTypeProvider, 
  ReadSchema extends BaseSchemaType<TypeProvider>,
  UpdateSchema extends BaseSchemaType<TypeProvider>,
  CreateSchema extends BaseSchemaType<TypeProvider>,
  TRead = ResolveSchemaType<TypeProvider, ReadSchema>,
  TUpdate = ResolveSchemaType<TypeProvider, UpdateSchema>,
  TCreate = ResolveSchemaType<TypeProvider, CreateSchema>,
> {
  private readonly _compiledReadSchema: ResolveCompiledSchema<TypeProvider, ReadSchema>;
  private readonly _compiledUpdateSchema: ResolveCompiledSchema<TypeProvider, UpdateSchema>;
  private readonly _compiledCreateSchema: ResolveCompiledSchema<TypeProvider, CreateSchema>;
  
  constructor(
    private readonly _collection: MongoCollection,
    private readonly _config: CollectionConfig<TypeProvider, ReadSchema, UpdateSchema, CreateSchema>
  ) {
    this._compiledReadSchema = _config.schemaProvider.compile(_config.readSchema);
    this._compiledUpdateSchema = _config.schemaProvider.compile(_config.updateSchema);
    this._compiledCreateSchema = _config.schemaProvider.compile(_config.createSchema);
  }

  public get readSchema(): ReadSchema {
    return this._config.readSchema;
  }

  public get updateSchema(): UpdateSchema {
    return this._config.updateSchema;
  }

  public get createSchema(): CreateSchema {
    return this._config.createSchema;
  }

  public validateRead(input: unknown): input is TRead {
    return this._config.schemaProvider.validate(this._compiledReadSchema, input);
  }

  public validateUpdate(input: unknown): input is TUpdate {
    return this._config.schemaProvider.validate(this._compiledUpdateSchema, input);
  }

  public validateCreate(input: unknown): input is TCreate {
    return this._config.schemaProvider.validate(this._compiledCreateSchema, input);
  }
  
  public aggregate<R extends Document>(
    pipeline?: Document[],
    options?: AggregateOptions
  ): AggregationCursor<R> {
    return this._collection.aggregate(pipeline, options);
  }

  public async bulkWrite(operations: AnyBulkWriteOperation<T>[], options: BulkWriteOptions): Promise<BulkWriteResult> {
    return 
  }
  public count() {}
  public countDocuments() {}
  public createIndex() {}
  public createIndexes() {}
  public createSearchIndex() {}
  public createSearchIndexes() {}
  public deleteMany() {}
  public deleteOne() {}
  public distinct() {}
  public drop() {}
  public dropIndex() {}
  public dropIndexes() {}
  public dropSearchIndex() {}
  public estimatedDocumentCount() {}
  public find() {}
  public findOne() {}
  public findOneAndDelete() {}
  public findOneAndReplace() {}
  public findOneAndUpdate() {}
  public indexExists() {}
  public indexInformation() {}
  public indexes() {}
  public initializeOrderedBulkOp() {}
  public initializeUnorderedBulkOp() {}
  public insertMany() {}
  public insertOne() {}
  public isCapped() {}
  public listIndexes() {}
  public listSearchIndexes() {}
  public options() {}
  public rename() {}
  public replaceOne() {}
  public updateMany() {}
  public updateOne() {}
  public updateSearchIndex() {}
  public watch() {}
}

interface TypeBoxSchemaTypeProvider extends SchemaTypeProvider {
  readonly baseSchemaType: TSchema;
  readonly baseCompiledSchemaType: TypeCheck<TSchema>;

  readonly compiledSchemaType: this['schemaType'] extends TSchema ? TypeCheck<this['schemaType']> : unknown;

  readonly resolveSchemaType: this['schemaType'] extends TSchema ? Static<this['schemaType']> : unknown;

  readonly resolveCompiledSchema: this['schemaType'] extends TSchema ? Static<this['schemaType']> : this['compiledSchemaType'] extends TypeCheck<infer Schema> ? Static<Schema> : unknown;
}

declare const TypeBoxProvider: SchemaProvider<TypeBoxSchemaTypeProvider>;

const coll = new Collection(
  TypeBoxProvider, 
  Type.Object({ 
    thing: Type.String(), 
    bing: Type.Number()
  }),
);

const a = coll.create({ foo: 'bar'});
