#import "SPDBMapEntry.h"
#import "SPDBMappingFactory.h"

@implementation SPDBMappingFactory

- (RKObjectMapping *)createObjectMappingForMapEntry
{
    RKObjectMapping *mapping = [RKObjectMapping mappingForClass:[SPDBMapEntry class]];
    [mapping addAttributeMappingsFromDictionary:@{
        @"id":                      @"id",
        @"wkt":                     @"wkt",
        @"coordinates.latitude":    @"latitude",
        @"coordinates.longitude":   @"longitude"
    }];
    
    return mapping;
}

@end
