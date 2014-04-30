#import "SPDBMapEntry.h"
#import "SPDBMappingFactory.h"
#import "SPDBRoute.h"

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

- (RKObjectMapping *)createObjectMappingForRoute
{
    RKObjectMapping *mapping = [RKObjectMapping mappingForClass:[SPDBRoute class]];
    [mapping addAttributeMappingsFromDictionary:@{
        @"id": @"id",
        @"routeFrom.id": @"routeFrom.id",
        @"routeFrom.wkt":
    }];
    
    return mapping;
}

@end
