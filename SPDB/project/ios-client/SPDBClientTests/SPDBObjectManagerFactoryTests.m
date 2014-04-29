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
#import "SPDBShortestPathRequest.h"
#import <XCTAsyncTestCase/XCTAsyncTestCase.h>

@interface SPDBObjectManagerFactoryTests : XCTAsyncTestCase

@property (nonatomic, strong) RKMappingResult *mappingResult;

@end

@implementation SPDBObjectManagerFactoryTests

- (void)setUp
{
    [super setUp];
}

- (void)tearDown
{
    [super tearDown];
}

- (void)testObjectManagerForNearestMapEntry
{
    RKObjectManager *objectManager = [self createObjectManager];
    SPDBMapEntry *mapEntry = [SPDBMapEntry mapEntryWithLatitude:[NSNumber numberWithDouble:52.220067] andLongitude:[NSNumber numberWithDouble:21.012119]];
    
    [self prepare];
    
    [objectManager getObject:mapEntry path:nil parameters:nil success:^(RKObjectRequestOperation *operation, RKMappingResult *mappingResult) {
        self.mappingResult = mappingResult;
        [self notify:kXCTUnitWaitStatusSuccess];
    } failure:^(RKObjectRequestOperation *operation, NSError *error) {
        [self notify:kXCTUnitWaitStatusFailure];
    }];
    
    [self waitForStatus:kXCTUnitWaitStatusSuccess timeout:10.0];
    
    XCTAssertNotNil(self.mappingResult, @"No mapping result found");
    XCTAssertEqual([[self.mappingResult array] count], 1, @"Response mapping failed");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.220067], [[self.mappingResult array][0] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.012119], [[self.mappingResult array][0] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22006700 21.01211900 )", [[self.mappingResult array][0] wkt], @"Wkt is not right!");
}

- (RKObjectManager *)createObjectManager
{
    SPDBObjectManagerFactory *objectManagerFactory = [SPDBObjectManagerFactory new];
    RKObjectManager *objectManager = [objectManagerFactory createObjectManager];
    
    return objectManager;
}

- (void)testObjectManagerForShortestPath
{
    RKObjectManager *objectManager = [self createObjectManager];
    SPDBShortestPathRequest *shortestPathRequest = [SPDBShortestPathRequest requestWithStartingNodeId:[NSNumber numberWithLong:0] andFinishingNodeId:[NSNumber numberWithLong:28]];
    
    [self prepare];
    
    [objectManager getObjectsAtPathForRouteNamed:@"ShortestPath" object:shortestPathRequest parameters:nil success:^(RKObjectRequestOperation *operation, RKMappingResult *mappingResult) {
        self.mappingResult = mappingResult;
        [self notify:kXCTUnitWaitStatusSuccess];
    } failure:^(RKObjectRequestOperation *operation, NSError *error) {
        [self notify:kXCTUnitWaitStatusSuccess];
    }];
    
    [self waitForStatus:kXCTUnitWaitStatusSuccess timeout:10.0];
    
    XCTAssertNotNil(self.mappingResult, @"No mapping result found");
    XCTAssertEqual([[self.mappingResult array] count], 5, @"Response mapping failed");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.220067], [[self.mappingResult array][0] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.012119], [[self.mappingResult array][0] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22006700 21.01211900 )", [[self.mappingResult array][0] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.219893], [[self.mappingResult array][1] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.018152], [[self.mappingResult array][1] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.21989300 21.01815200 )", [[self.mappingResult array][1] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.223232], [[self.mappingResult array][2] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.015984], [[self.mappingResult array][2] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22323200 21.01598400 )", [[self.mappingResult array][2] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.226229], [[self.mappingResult array][3] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.014161], [[self.mappingResult array][3] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22622900 21.01416100 )", [[self.mappingResult array][3] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.230014], [[self.mappingResult array][4] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.011886], [[self.mappingResult array][4] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.23001400 21.01188600 )", [[self.mappingResult array][4] wkt], @"Wkt is not right!");
}

@end
