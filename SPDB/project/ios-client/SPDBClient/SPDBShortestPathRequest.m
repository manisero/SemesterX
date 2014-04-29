#import "SPDBShortestPathRequest.h"

@implementation SPDBShortestPathRequest

+ (SPDBShortestPathRequest *)requestWithStartingNodeId:(NSNumber *)startingNodeId andFinishingNodeId:(NSNumber *)finishingNodeId
{
    SPDBShortestPathRequest *request = [SPDBShortestPathRequest new];
    request.startingNodeId = startingNodeId;
    request.finishingNodeId = finishingNodeId;
    
    return request;
}

@end
