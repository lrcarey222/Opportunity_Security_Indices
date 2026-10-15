import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, render } from '@testing-library/react';
import { bareAcronyms, bareAcronymsInText } from './acronyms.ts';

afterEach(cleanup);

describe('bareAcronymsInText', () => {
  it('flags a bare acronym and keeps the text for the message', () => {
    expect(bareAcronymsInText('PSI rank')).toEqual(['PSI in "PSI rank"']);
    expect(bareAcronymsInText('ES and EO by sector')).toEqual(['ES in "ES and EO by sector"', 'EO in "ES and EO by sector"']);
  });

  it('allows the acronym in parentheses directly after its full name', () => {
    expect(bareAcronymsInText('Partnership Strength Index (PSI)')).toEqual([]);
    expect(bareAcronymsInText('Concentration (HHI)')).toEqual([]);
    expect(bareAcronymsInText('Factory projects (CM-FDI) by destination')).toEqual([]);
    expect(bareAcronymsInText('Indo-Pacific Economic Framework (IPEF)')).toEqual([]);
  });

  it('flags parentheses after another name, and acronyms that are not the whole parenthesis', () => {
    expect(bareAcronymsInText('Supplier score (PSI)')).toHaveLength(1);
    expect(bareAcronymsInText('Strength (PSI, best five)')).toHaveLength(1);
  });

  it('matches whole tokens only, case-sensitively', () => {
    expect(bareAcronymsInText('Estonia, Spain, CATL and POLAND; costs, es, the SIM card')).toEqual([]);
    expect(bareAcronymsInText('FDI flows')).toEqual([]);
    expect(bareAcronymsInText('CM-FDI projects')).toEqual(['CM-FDI in "CM-FDI projects"']);
    // allowed everywhere
    expect(bareAcronymsInText('EU, UN, GDP, USD, NATO, G7 and TWh')).toEqual([]);
  });
});

describe('bareAcronyms (DOM)', () => {
  it('scans headings, headers, labels, buttons, nav links, tabs, eyebrows, captions and tags', () => {
    const { container } = render(
      <div>
        <h1>Energy Security</h1>
        <h2>PSI partners</h2>
        <p>Running text may say PSI after the full name earlier on the page.</p>
        <table>
          <thead>
            <tr>
              <th>FS best five</th>
              <th>
                Partnership Strength Index (<abbr title="Partnership Strength Index">PSI</abbr>)
              </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>
                <abbr title="Partnership Strength Index">PSI</abbr>
              </td>
            </tr>
          </tbody>
        </table>
        <label htmlFor="x">SI rank</label>
        <input id="x" />
        <button type="button" aria-label="Reset ES weights">
          x
        </button>
        <nav>
          <a href="/a">CAT ratings</a>
        </nav>
        <div role="tab">Concentration (HHI)</div>
        <div className="eyebrow">TRL</div>
        <figure>
          <figcaption>Climate Action Tracker (CAT)</figcaption>
        </figure>
        <span className="tag">CM-FDI</span>
        <span aria-hidden="true" className="tag">
          PO
        </span>
      </div>,
    );
    expect(bareAcronyms(container)).toEqual([
      'PSI in "PSI partners"',
      'FS in "FS best five"',
      'SI in "SI rank"',
      'ES in "Reset ES weights"',
      'CAT in "CAT ratings"',
      'TRL in "TRL"',
      'CM-FDI in "CM-FDI"',
    ]);
  });

  it('a page that spells its terms out passes', () => {
    const { container } = render(
      <div>
        <h1>Partners</h1>
        <h2>Partnership Strength</h2>
        <table>
          <thead>
            <tr>
              <th>Friendshore (best five)</th>
              <th>Partnership Opportunity (best five)</th>
              <th>Development Potential</th>
            </tr>
          </thead>
        </table>
        <span className="tag">Energy Security weights</span>
      </div>,
    );
    expect(bareAcronyms(container)).toEqual([]);
  });
});
