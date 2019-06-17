import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import {}
import { TestBed } from '@angular/core/testing';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';

let sutService: ServiceUnderTestService;
let httpClient: HttpClient;
let httpTestingController: HttpTestingController;

describe('I want to test the thing', () => {
  beforeEach(() => {
    httpClientSpy = jasmine.createSpyObj('HttpClient', ['get']);

    TestBed.configureTestingModule({
      imports: [ HttpClientTestingModule ],
      providers: [
        ServiceUnderTestService
      ]
    });

    // Inject the http service and test controller for each test
    httpClient = TestBed.get(HttpClient);
    httpTestingController = TestBed.get(HttpTestingController);
    sutService = TestBed.get(ServiceUnderTestService)
  });

  afterEach(() => {
    // Verify there are no outstanding
    httpTestingController.verify();
  });

  it('can test HttpClient.get', () => {
    const testData: Data = {name: 'Test Data'};

    // Make an HTTP GET request
    httpClient.get<Data>(testUrl)
      .subscribe(data =>
        // When observable resolves, result should match test data
        expect(data).toEqual(testData),
      fail);

    // The following `expectOne()` will match the request's URL.
    // If no requests or multiple requests matched that URL
    // `expectOne()` would throw.
    const req = httpTestingController.expectOne('/data');

    // Assert that the request is a GET.
    expect(req.request.method).toEqual('GET');

    // Respond with mock data, causing Observable to resolve.
    // Subscribe callback asserts that correct data was returned.
    req.flush(testData);
  });
});
