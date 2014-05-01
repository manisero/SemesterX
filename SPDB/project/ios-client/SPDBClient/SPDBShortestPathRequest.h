#import <Foundation/Foundation.h>

@interface SPDBShortestPathRequest : NSObject

@property (nonatomic, strong) NSNumber *startingNodeId;
@property (nonatomic, strong) NSNumber *finishingNodeId;

+ (SPDBShortestPathRequest *)requestWithStartingNodeId:(NSNumber *)startingNodeId andFinishingNodeId:(NSNumber *)finishingNodeId;

@end