#import "SPDBConfigurationProvider.h"
#import "SPDBMapEntry.h"
#import "SPDBMappingFactory.h"
#import "SPDBObjectManagerFactory.h"

@implementation SPDBObjectManagerFactory

- (RKObjectManager *)createObjectManager
{
    SPDBMappingFactory *mappingFactory = [SPDBMappingFactory new];
    
    RKObjectManager *manager = [RKObjectManager managerWithBaseURL:[NSURL URLWithString:[SPDBConfigurationProvider getServiceAddress]]];
    [manager.router.routeSet addRoute:[RKRoute routeWithClass:[SPDBMapEntry class] pathPattern:@"entry/nearest/:latitude/:longitude" method:RKRequestMethodGET]];
    [manager.router.routeSet addRoute:[RKRoute routeWithName:@"ShortestPath" pathPattern:@"entry/shortestPath/:startingNodeId/:finishingNodeId" method:RKRequestMethodGET]];
    [manager addResponseDescriptor:[RKResponseDescriptor responseDescriptorWithMapping:[mappingFactory createObjectMappingForMapEntry] method:RKRequestMethodGET pathPattern:nil keyPath:@"" statusCodes:[NSIndexSet indexSetWithIndex:200]]];
    
    return manager;
}

@end
