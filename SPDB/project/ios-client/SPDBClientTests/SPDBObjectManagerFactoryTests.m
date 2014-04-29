//
//  SPDBObjectManagerFactoryTests.m
//  SPDBClient
//
//  Created by Jakub Turek on 29.04.2014.
//  Copyright (c) 2014 Jakub Turek. All rights reserved.
//

#import <RestKit/RestKit.h>
#import "SPDBMapEntry.h"
#import "SPDBObjectManagerFactory.h"
#import <XCTAsyncTestCase/XCTAsyncTestCase.h>

@interface SPDBObjectManagerFactoryTests : XCTAsyncTestCase

@property (nonatomic, strong) RKObjectRequestOperation *operation;

@end

@implementation SPDBObjectManagerFactoryTests

- (void)setUp
{
    [super setUp];
    self.operation = nil;
}

- (void)tearDown
{
    [super tearDown];
}

- (void)testObjectManagerForNearestMapEntry
{
    SPDBObjectManagerFactory *objectManagerFactory = [SPDBObjectManagerFactory new];
    RKObjectManager *objectManager = [objectManagerFactory createObjectManager];
    SPDBMapEntry *mapEntry = [SPDBMapEntry mapEntryWithLatitude:[NSNumber numberWithDouble:52.220067] andLongitude:[NSNumber numberWithDouble:21.012119]];
    
    [self prepare];
    
    [objectManager getObject:mapEntry path:nil parameters:nil success:^(RKObjectRequestOperation *operation, RKMappingResult *mappingResult) {
        self.operation = operation;
        [self notify:kXCTUnitWaitStatusSuccess];
    } failure:^(RKObjectRequestOperation *operation, NSError *error) {
        [self notify:kXCTUnitWaitStatusFailure];
    }];
    
    [self waitForStatus:kXCTUnitWaitStatusSuccess timeout:10.0];
    
    XCTAssertEqual(1, [self.operation.mappingResult count], @"No mapping result found");
    
    SPDBMapEntry *result = [self.operation.mappingResult array][0];
    
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.220067], result.latitude, @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.012119], result.longitude, @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22006700 21.01211900 )", result.wkt, @"Wkt is not right!");
}

@end
